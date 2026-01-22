# Google Drive Data Download Script
# This script downloads data files from Google Drive to the local data/ folder
# Uses OAuth authentication for secure access

# Load required packages
if (!require("googledrive")) install.packages("googledrive")
library(googledrive)

# Authenticate with Google Drive
# This will open a browser window for OAuth authentication
# The token will be cached locally for future use
drive_auth()

# Google Drive folder URL
# https://drive.google.com/drive/u/1/folders/1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-
gdrive_folder_id <- "1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-"

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# List all files in the Google Drive folder
cat("Fetching files from Google Drive...\n")
files <- drive_ls(as_id(gdrive_folder_id))

if (nrow(files) == 0) {
  cat("No files found in the Google Drive folder.\n")
} else {
  cat(paste0("Found ", nrow(files), " file(s) in Google Drive folder.\n\n"))

  # Download each file
  for (i in 1:nrow(files)) {
    file_name <- files$name[i]
    file_id <- files$id[i]

    cat(paste0("Downloading: ", file_name, "\n"))

    # Download to data/ folder
    drive_download(
      file = as_id(file_id),
      path = file.path("data", file_name),
      overwrite = TRUE
    )
  }

  cat("\nAll files downloaded successfully!\n")
  cat(paste0("Files saved to: ", normalizePath("data"), "\n"))
}

# List downloaded files
cat("\nDownloaded files:\n")
print(list.files("data"))
