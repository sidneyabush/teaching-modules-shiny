# Teaching Modules Shiny App

Interactive Shiny application for teaching modules.

## Setup

### 1. Install Required Packages

```r
install.packages(c("shiny", "googledrive"))
```

### 2. Download Data from Google Drive

The data files are stored in Google Drive and are downloaded to a local machine.

**Run the data download script:**

```r
source("data_download.R")
```

This will:
- Prompt you to authenticate with Google (OAuth)
- Download all files from the Google Drive folder
- Save them locally`

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
├── .gitignore            # Excludes sensitive info
└── README.md             # This file
```

<<<<<<< Updated upstream
=======
## Data Harmonization

The `1_Data_Harmonization.R` script processes the raw chemistry and discharge data to create a harmonized dataset for North American sites (US + Canada).

**Run the harmonization:**

```r
source("1_Data_Harmonization.R")
```

**Output files** (in Box data folder):
- `harmonized_north_america_partial.csv` - All 1,134 North American sites with partial data (many NAs)
- `harmonized_north_america_complete.csv` - 141 sites with complete data (RBI, RCS, climate, slope)
- `harmonization_summary.csv` - Summary statistics

**What the harmonization includes:**
- Filtered to North American sites (1,134 sites)
- RBI (Richards-Baker Flashiness Index) - 192 sites
- RCS (Recession Curve Slope) - 190 sites
- Köppen-Geiger climate classification - 173 sites
- Basin slope and elevation data - 148 sites
- Precipitation, temperature, evapotranspiration (time series)
- Land use/land cover percentages
- 141 sites with complete data across all metrics

## Data Location

Data files are stored in Box (not in this repository):
`/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data/`

>>>>>>> Stashed changes
## Data Source

Original data is in Google Drive, and may change as more sites are added/ data issues are resolved:
https://drive.google.com/drive/folders/1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-

## Download Scripts

- `data_download.R` - Downloads master chemistry and discharge files
- `download_driver_data.R` - Downloads climate, spatial, and land use data

## Notes
<<<<<<< Updated upstream
- Download data using `data_download.R` (downloads to Box location)
=======

- Data files are stored in Box, not in the git repository
- Download data using the download scripts (saves to Box location)
>>>>>>> Stashed changes
- OAuth authentication token is cached locally (excluded from git)
