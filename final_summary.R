data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data"

harmonized <- read.csv(file.path(data_path, "harmonized_north_america_partial.csv"),
                       stringsAsFactors = FALSE)

# Filter to sites within North America only
harmonized <- harmonized[
  (is.na(harmonized$Longitude) | (harmonized$Longitude >= -170 & harmonized$Longitude <= -50)) &
  (is.na(harmonized$Latitude) | (harmonized$Latitude >= 15 & harmonized$Latitude <= 85)),
]

complete_cases <- harmonized[complete.cases(
  harmonized[, c("RBI", "recession_slope", "ClimateZ", "basin_slope_mean_degree")]
), ]

summary_data <- data.frame(
  Metric = c(
    "Total sites",
    "Sites with RBI and RCS",
    "Sites with climate classification",
    "Sites with basin slope",
    "Sites with complete data (RBI+RCS+Climate+Slope)",
    "Number of columns in harmonized dataset"
  ),
  Count = c(
    nrow(harmonized),
    sum(!is.na(harmonized$RBI) & !is.na(harmonized$recession_slope)),
    sum(!is.na(harmonized$ClimateZ)),
    sum(!is.na(harmonized$basin_slope_mean_degree)),
    nrow(complete_cases),
    ncol(harmonized)
  )
)

write.csv(summary_data,
          file.path(data_path, "harmonization_summary.csv"),
          row.names = FALSE)

write.csv(complete_cases,
          file.path(data_path, "harmonized_north_america_complete.csv"),
          row.names = FALSE)
