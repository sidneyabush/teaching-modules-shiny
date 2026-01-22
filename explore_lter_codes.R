# Quick exploration to identify North American LTER codes

if (!require("librarian")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages("librarian")
}
librarian::shelf(dplyr)

data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data"

chem_data <- read.csv(file.path(data_path, "20260105_masterdata_chem.csv"),
                      stringsAsFactors = FALSE)

discharge_data <- read.csv(file.path(data_path, "20260106_masterdata_discharge.csv"),
                          stringsAsFactors = FALSE)

unique_lter_chem <- unique(chem_data$LTER)
unique_lter_discharge <- unique(discharge_data$LTER)

all_lter <- unique(c(unique_lter_chem, unique_lter_discharge))

north_american_lter <- c(
  "Canada",
  "USGS",
  "AND",
  "ARC",
  "BcCZO",
  "BNZ",
  "ColoradoAlpine",
  "CZO-Catalina Jemez",
  "Catalina Jemez",
  "EastRiverSFA",
  "GRO",
  "HBR",
  "Ipswitch(Carey)",
  "KNZ",
  "LMP",
  "LMP(Wymore)",
  "LUQ",
  "NWT",
  "PIE",
  "Sagehen",
  "Sagehen(Sullivan)",
  "UMR",
  "UMR(Jankowski)",
  "WalkerBranch",
  "Walker Branch"
)


write.csv(north_american_lter,
          file.path(data_path, "north_american_lter_codes.csv"),
          row.names = FALSE)
