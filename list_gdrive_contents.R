if (!require("googledrive")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages("googledrive")
}
library(googledrive)

drive_auth()

gdrive_folder_id <- "1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-"

list_folder_recursive <- function(folder_id, prefix = "") {
  items <- drive_ls(as_id(folder_id))

  if (nrow(items) == 0) {
    return()
  }

  for (i in 1:nrow(items)) {
    item_name <- items$name[i]
    item_type <- items$drive_resource[[i]]$mimeType

    if (item_type == "application/vnd.google-apps.folder") {
      list_folder_recursive(items$id[i], paste0(prefix, "  "))
    }
  }
}

list_folder_recursive(gdrive_folder_id)
