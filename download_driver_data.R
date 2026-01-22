if (!require("googledrive")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages("googledrive")
}
library(googledrive)

drive_auth()

gdrive_folder_id <- "1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-"
data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data"

download_specific_file <- function(file_path, local_path = data_path) {
  path_parts <- strsplit(file_path, "/")[[1]]
  current_folder_id <- gdrive_folder_id

  for (i in 1:(length(path_parts) - 1)) {
    folder_name <- path_parts[i]
    items <- drive_ls(as_id(current_folder_id))
    folder_match <- items[items$name == folder_name, ]

    if (nrow(folder_match) == 0) {
      stop(paste0("Folder not found: ", folder_name))
    }

    current_folder_id <- folder_match$id[1]

    local_subfolder <- file.path(local_path, paste(path_parts[1:i], collapse = "/"))
    if (!dir.exists(local_subfolder)) {
      dir.create(local_subfolder, recursive = TRUE)
    }
  }

  file_name <- path_parts[length(path_parts)]
  files <- drive_ls(as_id(current_folder_id))
  file_match <- files[files$name == file_name, ]

  if (nrow(file_match) == 0) {
    stop(paste0("File not found: ", file_name))
  }

  local_file_path <- file.path(local_path, file_path)

  drive_download(
    file = as_id(file_match$id[1]),
    path = local_file_path,
    overwrite = TRUE
  )
}

download_root_file <- function(file_name, local_path = data_path) {
  items <- drive_ls(as_id(gdrive_folder_id))
  file_match <- items[items$name == file_name, ]

  if (nrow(file_match) == 0) {
    stop(paste0("File not found in root: ", file_name))
  }

  drive_download(
    file = as_id(file_match$id[1]),
    path = file.path(local_path, file_name),
    overwrite = TRUE
  )
}

download_specific_file("Driver_Variables/Data Release 2/Data Harmonization/Additional files needed/Koeppen_Geiger_2.csv")
download_specific_file("Driver_Variables/Data Release 2/all-data_si-extract_2_20250325.csv")
download_root_file("DSi_LULC_filled_interpolated_Simple.csv")
