# Data Harmonization for Teaching Modules
# Adapted from LTER-SiSyn-Spatial-Controls harmonization workflow
#
# Inputs:
#   - 20260105_masterdata_chem.csv (chemistry data)
#   - 20260106_masterdata_discharge.csv (discharge data)
#
# Outputs:
#   - Harmonized dataset with:
#     * North American sites only (US + Canada)
#     * RBI (Richards-Baker Flashiness Index)
#     * Recession Curve Slope (RCS)
#     * Climate variables per site
#     * Land use variables per site

rm(list = ls())

if (!require("librarian")) install.packages("librarian")
librarian::shelf(dplyr, ggplot2, data.table, lubridate, tidyr, stringr, readr)

data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data"

# Helper function to create and standardize Stream_ID
create_stream_id <- function(df) {
  df %>%
    mutate(
      Stream_ID = paste(LTER, Stream_Name, sep = "_"),
      Stream_ID = str_trim(Stream_ID),
      Stream_ID = str_replace_all(Stream_ID, "\\s+", "_")
    )
}

# Load chemistry data
chem_data <- read.csv(file.path(data_path, "20260105_masterdata_chem.csv"),
                      stringsAsFactors = FALSE)

# Load discharge data
discharge_data <- read.csv(file.path(data_path, "20260106_masterdata_discharge.csv"),
                          stringsAsFactors = FALSE)

# Filter to North American sites (US and Canada)
north_american_lter <- c(
  "Canada", "USGS", "AND", "ARC", "BcCZO", "BNZ", "ColoradoAlpine",
  "CZO-Catalina Jemez", "Catalina Jemez", "EastRiverSFA", "GRO", "HBR",
  "Ipswitch(Carey)", "KNZ", "LMP", "LMP(Wymore)", "LUQ", "NWT", "PIE",
  "Sagehen", "Sagehen(Sullivan)", "UMR", "UMR(Jankowski)",
  "WalkerBranch", "Walker Branch"
)

chem_na <- chem_data %>%
  filter(LTER %in% north_american_lter) %>%
  create_stream_id()

discharge_na <- discharge_data %>%
  filter(LTER %in% north_american_lter) %>%
  create_stream_id() %>%
  rename(Q = Qcms) %>%
  mutate(Date = as.Date(Date))

# Calculate RBI (Richards-Baker Flashiness Index)
rbi_results <- discharge_na %>%
  group_by(Stream_ID, LTER, Stream_Name) %>%
  arrange(Date) %>%
  mutate(
    dQ = Q - lag(Q),
    abs_dQ = abs(dQ)
  ) %>%
  filter(!is.na(abs_dQ)) %>%
  summarise(
    n_days = n(),
    total_discharge = sum(Q, na.rm = TRUE),
    total_change = sum(abs_dQ, na.rm = TRUE),
    RBI = total_change / total_discharge,
    .groups = "drop"
  ) %>%
  filter(n_days >= 365)

# Calculate Recession Curve Slope (RCS)
Q_diff <- discharge_na %>%
  arrange(Stream_ID, Date) %>%
  group_by(Stream_ID) %>%
  mutate(
    dQ = Q - lag(Q),
    change_dQ = Q / lag(Q),
    dQ_dt = dQ / as.numeric(Date - lag(Date))
  ) %>%
  filter(!is.na(dQ_dt)) %>%
  filter(change_dQ >= 0.7)

recession_data <- Q_diff %>%
  filter(dQ < 0) %>%
  mutate(recession_slope = -dQ_dt) %>%
  filter(is.finite(recession_slope), recession_slope > 0)

recession_slopes <- recession_data %>%
  group_by(Stream_ID, LTER, Stream_Name) %>%
  summarise(
    n_recession_days = n(),
    recession_slope = if(n_recession_days >= 50) {
      tryCatch({
        lm_model <- lm(log(recession_slope) ~ log(Q), data = pick(everything()))
        unname(coef(lm_model)[2])
      }, error = function(e) NA_real_)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(recession_slope), recession_slope >= 0)

# Merge RBI and RCS
discharge_metrics <- rbi_results %>%
  left_join(recession_slopes %>% select(Stream_ID, recession_slope, n_recession_days),
            by = "Stream_ID")

# Get unique site identifiers from chemistry data for merging
sites_info <- chem_na %>%
  select(Stream_ID, LTER, Stream_Name) %>%
  distinct()

# Merge discharge metrics with sites
sites_with_discharge <- sites_info %>%
  left_join(discharge_metrics, by = c("Stream_ID", "LTER", "Stream_Name"))

# Load Köppen-Geiger climate classification
kg_data <- read.csv(file.path(data_path, "Driver_Variables/Data Release 2/Data Harmonization/Additional files needed/Koeppen_Geiger_2.csv"),
                    stringsAsFactors = FALSE) %>%
  create_stream_id() %>%
  select(Stream_ID, ClimateZ, Latitude, Longitude, Name)

# Load spatial drivers
spatial_drivers <- read.csv(file.path(data_path, "Driver_Variables/Data Release 2/all-data_si-extract_2_20250325.csv"),
                           stringsAsFactors = FALSE) %>%
  create_stream_id() %>%
  select(Stream_ID, LTER, Stream_Name,
         basin_slope_mean_degree, basin_slope_median_degree,
         elevation_mean_m, elevation_median_m,
         starts_with("precip_"), starts_with("temp_"),
         starts_with("evapotrans_"),
         starts_with("land_"), major_land, major_rock, major_soil)

# Load LULC data and pivot to wide format
lulc_data <- read.csv(file.path(data_path, "DSi_LULC_filled_interpolated_Simple.csv"),
                      stringsAsFactors = FALSE) %>%
  filter(Year >= 2002, Year <= 2022) %>%
  mutate(
    LandClass_sum = if_else(
      is.na(LandClass_sum) | LandClass_sum == 0,
      LandClass_sum,
      LandClass_sum * 100
    )
  ) %>%
  filter(Simple_Class != "Filled_Value") %>%
  pivot_wider(
    names_from = Simple_Class,
    values_from = LandClass_sum,
    names_prefix = "land_"
  )

# Calculate average land use per site (across all years)
lulc_avg <- lulc_data %>%
  mutate(Stream_ID = paste0(Stream_Name, "_", Stream_Name)) %>%
  group_by(Stream_Name) %>%
  summarise(across(starts_with("land_"), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(
    major_land_lulc = apply(select(., starts_with("land_")), 1, function(x) {
      if(all(is.na(x))) NA_character_
      else names(x)[which.max(x)]
    })
  )

# Merge climate data
harmonized_data <- sites_with_discharge %>%
  left_join(kg_data, by = "Stream_ID")

# Merge spatial drivers
harmonized_data <- harmonized_data %>%
  left_join(spatial_drivers, by = c("Stream_ID", "LTER", "Stream_Name"))

# Merge LULC data
harmonized_data <- harmonized_data %>%
  left_join(lulc_avg, by = "Stream_Name")

# Filter to sites within North America (exclude Russian GRO sites)
# North America bounds: Longitude -170 to -50, Latitude 15 to 85
# This includes US, Canada, Alaska, Puerto Rico, and other US territories
harmonized_data <- harmonized_data %>%
  filter(
    is.na(Longitude) | (Longitude >= -170 & Longitude <= -50),
    is.na(Latitude) | (Latitude >= 15 & Latitude <= 85)
  )

# Save harmonized dataset
output_file <- file.path(data_path, "harmonized_north_america_partial.csv")
write.csv(harmonized_data, output_file, row.names = FALSE)
