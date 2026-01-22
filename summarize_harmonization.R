data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data"

harmonized <- read.csv(file.path(data_path, "harmonized_north_america_partial.csv"),
                       stringsAsFactors = FALSE)

total_sites <- nrow(harmonized)
sites_with_discharge <- sum(!is.na(harmonized$n_days))
sites_with_rbi <- sum(!is.na(harmonized$RBI))
sites_with_recession <- sum(!is.na(harmonized$recession_slope))
sites_with_both <- sum(!is.na(harmonized$RBI) & !is.na(harmonized$recession_slope))

lter_summary <- aggregate(cbind(!is.na(RBI), !is.na(recession_slope)) ~ LTER,
                          data = harmonized, FUN = sum)
colnames(lter_summary) <- c("LTER", "Sites_with_RBI", "Sites_with_RCS")
lter_counts <- table(harmonized$LTER)
lter_summary$Total_Sites <- lter_counts[lter_summary$LTER]

lter_summary <- lter_summary[order(-lter_summary$Sites_with_RBI), ]
