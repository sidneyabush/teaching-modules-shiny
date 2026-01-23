# Teaching Modules Shiny App

Interactive Shiny application for teaching modules.

## Setup

### 1. Install Required Packages

```r
install.packages(c("shiny", "bslib", "googledrive", "dplyr", "ggplot2", "leaflet", "plotly", "viridis"))
```

### 2. Download Data from Google Drive

The data files are stored in Google Drive and are downloaded to Box.

**Run the data download scripts:**

```r
source("data_download.R")
source("download_driver_data.R")
```

This will:
- Prompt you to authenticate with Google (OAuth)
- Download chemistry, discharge, climate, and land use files
- Save them to Box

**Note:** You only need to download the data once. The authentication token will be cached locally.

### 3. Run Data Harmonization

```r
source("1_Data_Harmonization.R")
source("add_snow_precip.R")
source("final_summary.R")
```

### 4. Run the Shiny App

```r
shiny::runApp()
```

Or open `app.R` in RStudio and click "Run App".

## Project Structure

```
teaching-modules-shiny/
├── app.R                       # Main Shiny application
├── 1_Data_Harmonization.R      # Data harmonization script
├── data_download.R             # Download chemistry and discharge data
├── download_driver_data.R      # Download climate and spatial data
├── add_snow_precip.R          # Add snow fraction metrics
├── final_summary.R            # Generate summary statistics
├── .gitignore                  # Excludes sensitive info
└── README.md                   # This file
```

## Data Harmonization

The `1_Data_Harmonization.R` script processes the raw chemistry and discharge data to create a harmonized dataset for North American sites (US, Canada, and US territories only).

**Output files** (in Box data folder):
- `harmonized_north_america_partial.csv` - All North American sites with partial data (many NAs)
- `harmonized_north_america_complete.csv` - 137 sites with complete data (RBI, RCS, climate, slope)
- `harmonization_summary.csv` - Summary statistics

**What the harmonization includes:**
- Filtered to North American sites (US, Canada, and US territories only)
- Geographic bounds: Longitude -170° to -50°, Latitude 15° to 85°
- Excludes Russian and other non-North American sites
- RBI (Richards-Baker Flashiness Index) - 188 sites
- RCS (Recession Curve Slope) - 186 sites
- Köppen-Geiger climate classification
- Basin slope and elevation data
- Precipitation, temperature, evapotranspiration (time series)
- Land use/land cover percentages
- Snow fraction metrics
- 137 sites with complete data across all metrics

## Data Location

Data files are stored in Box (not in this repository):
`/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data/`

## Data Source

Original data is in Google Drive:
https://drive.google.com/drive/folders/1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-

## Notes

- Data files are stored in Box, not in the git repository
- Download data using the download scripts (saves to Box location)
- OAuth authentication token is cached locally (excluded from git)
- Only US, Canadian, and US territory sites are included (Russian sites filtered out)
