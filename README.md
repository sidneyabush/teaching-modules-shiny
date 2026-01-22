# Teaching Modules Shiny App

Interactive Shiny application for teaching modules.

## Setup

### 1. Install Required Packages

```r
install.packages(c("shiny", "googledrive"))
```

### 2. Download Data from Google Drive

The data files are stored in Google Drive and need to be downloaded before running the app.

**Run the data download script:**

```r
source("data_download.R")
```

This will:
- Prompt you to authenticate with Google (OAuth)
- Download all files from the Google Drive folder
- Save them to the `data/` directory

**Note:** You only need to download the data once. The authentication token will be cached locally.

### 3. Run the Shiny App

```r
shiny::runApp()
```

Or open `app.R` in RStudio and click "Run App".

## Project Structure

```
teaching-modules-shiny/
├── app.R                  # Main Shiny application
├── data_download.R        # Script to download data from Google Drive
├── data/                  # Data folder (files downloaded here, not tracked in git)
├── .gitignore            # Excludes data files and sensitive info
└── README.md             # This file
```

## Data Source

Data is stored in Google Drive:
https://drive.google.com/drive/folders/1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-

## Notes

- Data files are **not tracked in git** (excluded via `.gitignore`)
- Each user must download the data using `data_download.R`
- OAuth authentication token is cached locally (also excluded from git)
