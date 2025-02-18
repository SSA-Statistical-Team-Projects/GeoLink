# GeoLink 🌍

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)]()
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![GitHub issues](https://img.shields.io/github/issues/your-username/geolink)]()

GeoLink is an R package that provides easy access to various geospatial datasets, allowing seamless integration with your spatial data or surveys.

## 📋 Table of Contents
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Examples](#examples)
- [Documentation](#documentation)
- [Contributing](#contributing)
- [License](#license)

## ✨ Features

Download and process:
- CHIRPS rainfall data
- Night Time Light (NTL) data
- Population data (WorldPop)
- Elevation data
- Building data (WorldPop)
- CMIP6 climate model data
- Cropland data
- WorldClim climate data
- Terraclimate data
- Land Use Land Cover data
- Points of Interest (OpenStreetMap)
- Electrification access data (HREA)
- OpenCellID data

## 🚀 Installation

```R
# Install devtools if you haven't already
install.packages("devtools")

# Install GeoLink
devtools::install_github("your-username/GeoLink")

# Load the package
library(GeoLink)
```

## 📊 Usage

Most functions require either:
- A shapefile (`shp_dt` or `shp_fn`)
- OR a survey dataset (`survey_dt` or `survey_fn`) with coordinates

Common parameters:
```R
geolink_function(
    shp_dt = your_shapefile,          # sf object containing polygons
    grid_size = 1000,                 # Grid size in meters
    survey_dt = your_survey,          # Survey data with coordinates
    buffer_size = 1000,               # Buffer size in meters
    extract_fun = "mean"              # Aggregation function
)
```

## 📝 Examples

### Rainfall Data (CHIRPS)
```R
df <- geolink_chirps(
    time_unit = "month",
    start_date = "2020-01-01",
    end_date = "2020-03-01",
    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
    grid_size = 1000,
    extract_fun = "mean"
)
```

Output:
```
Global Rainfall Raster Downloaded
Process Complete!!!
# Returns data.frame with rainfall_month1, rainfall_month2, etc.
```

### Night Time Lights
```R
df <- geolink_ntl(
    time_unit = "month",
    start_date = "2020-01-01",
    end_date = "2020-03-01",
    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
    indicator = "avg_rade9h",
    grid_size = 1000
)
```

Output:
```
Global NTL Raster Downloaded
Process Complete!!!
# Returns data.frame with ntl_month1_avg_rade9h, etc.
```

### Population Data
```R
df <- geolink_population(
    start_year = 2018,
    end_year = 2019,
    iso_code = "NGA",
    shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
    grid_size = 1000
)
```

Output:
```
Population Raster Processed
Process Complete!!!
# Returns data.frame with population_2018, population_2019
```

## 📚 Documentation

For detailed documentation of each function, use R's help system:
```R
?geolink_chirps
?geolink_ntl
?geolink_population
```

### Memory Considerations
- Some functions process large raster files
- Use `use_resampling = TRUE` for large areas
- Consider chunking large areas into smaller regions

### Error Handling
The package includes comprehensive error checking:
```R
# Invalid file format
df <- geolink_population(survey_fn = "invalid.txt")
# Error: "Unsupported file format. Please provide .dta or .csv file"

# CRS mismatch
df <- geolink_chirps(shp_dt = invalid_crs_data)
# Message: "Reprojecting from XXXX to EPSG:4326"
```

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## 📫 Contact

Your Name - [@your_twitter](https://twitter.com/your_twitter) - email@example.com

Project Link: [https://github.com/your-username/geolink](https://github.com/your-username/geolink)

---
Made with ❤️ by [The World Bank Group]
