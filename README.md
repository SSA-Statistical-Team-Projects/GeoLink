# GeoLink 🌍


<!-- badges: start -->
[![R-CMD-check](https://github.com/SSA-Statistical-Team-Projects/GeoLink/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SSA-Statistical-Team-Projects/GeoLink/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![GitHub issues](https://img.shields.io/github/issues/SSA-Statistical-Team-Projects/geolink)](https://github.com/SSA-Statistical-Team-Projects/geolink/issues)
<!-- badges: end -->

GeoLink is an R package that provides easy access to various geospatial datasets, allowing seamless integration with your spatial data or household surveys.

## 📋 Table of Contents
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Data Visualization Examples](#data-visualization-examples)
- [Basic Usage Examples](#basic-usage-examples)
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

# Install required visualization packages
install.packages(c("ggplot2", "sf", "leaflet", "viridis"))

# Install GeoLink
devtools::install_github("SSA-Statistical-Team-Projects/GeoLink")

# Load the package
library(GeoLink)
```

## 📊 Usage

Most functions require either:
- A shapefile (`shp_dt` or `shp_fn`)
- OR a household survey dataset (`survey_dt` or `survey_fn`) with coordinates

## 🗺️ Data Visualization Examples

### 1. Rainfall Visualization
```R
library(ggplot2)
library(sf)
library(viridis)

# Get rainfall data
rainfall_map <- geolink_chirps(
    time_unit = "month",
    start_date = "2020-01-01",
    end_date = "2020-01-31",
    shp_dt = nigeria_states[nigeria_states$ADM1_EN == "Abia",],
    grid_size = 1000
)

# Create beautiful rainfall map
ggplot(rainfall_map) +
    geom_sf(aes(fill = rainfall_month1)) +
    scale_fill_viridis_c(
        name = "Rainfall (mm)",
        option = "plasma"
    ) +
    theme_minimal() +
    labs(
        title = "January 2020 Rainfall Distribution",
        subtitle = "Abia State, Nigeria",
        caption = "Data source: CHIRPS"
    ) +
    theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "right"
    )
```

![Rainfall Map Example](/assets/images/rainfall_map.png)

### 2. Night Time Lights Analysis
```R
# Get night time lights data
ntl_map <- geolink_ntl(
    time_unit = "annual",
    start_date = "2020-01-01",
    end_date = "2020-12-31",
    shp_dt = nigeria_states[nigeria_states$ADM1_EN == "Lagos",],
    indicator = "average_masked",
    grid_size = 1000
)

# Create illuminating visualization
ggplot(ntl_map) +
    geom_sf(aes(fill = ntl_annual1average_masked)) +
    scale_fill_gradient(
        low = "navy",
        high = "yellow",
        name = "Light Intensity"
    ) +
    theme_minimal() +
    labs(
        title = "2020 Night Time Light Intensity",
        subtitle = "Lagos State, Nigeria",
        caption = "Data source: NASA Black Marble"
    ) +
    theme(
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 16, face = "bold")
    )
```

![Night Time Lights Map](/assets/images/ntl_map.png)

### 3. Interactive Population Density
```R
library(leaflet)

# Get population data
pop_data <- geolink_population(
    start_year = 2020,
    end_year = 2020,
    iso_code = "NGA",
    shp_dt = nigeria_states[nigeria_states$ADM1_EN == "Kano",],
    grid_size = 1000
)

# Create interactive population map
leaflet(pop_data) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    addPolygons(
        fillColor = ~colorQuantile("YlOrRd", population_2020)(population_2020),
        fillOpacity = 0.7,
        weight = 1,
        color = "#666",
        popup = ~paste(
            "<strong>Area:</strong>", 
            "<br>Population:", round(population_2020),
            "<br>Density:", round(population_2020/st_area(geometry))
        )
    ) %>%
    addLegend(
        "bottomright",
        title = "Population Density",
        pal = colorQuantile("YlOrRd", pop_data$population_2020),
        values = ~population_2020
    )
```

![Population Density Map](/assets/images/population_map.png)

### 4. Elevation Profile with Cropland Overlay
```R
# Combine elevation and cropland data
elevation_data <- geolink_elevation(
    iso_code = "NGA",
    shp_dt = nigeria_states[nigeria_states$ADM1_EN == "Plateau",],
    grid_size = 1000
)

cropland_data <- geolink_cropland(
    shp_dt = nigeria_states[nigeria_states$ADM1_EN == "Plateau",],
    grid_size = 1000
)

# Create combined visualization
ggplot(elevation_data) +
    geom_sf(aes(fill = elevation)) +
    geom_sf(data = cropland_data, aes(alpha = cropland), fill = "darkgreen") +
    scale_fill_gradient2(
        low = "darkgreen",
        mid = "yellowgreen",
        high = "brown",
        midpoint = median(elevation_data$elevation),
        name = "Elevation (m)"
    ) +
    scale_alpha_continuous(name = "Cropland Density") +
    theme_minimal() +
    labs(
        title = "Elevation Profile with Cropland Overlay",
        subtitle = "Plateau State, Nigeria"
    )
```

![Elevation and Cropland Map](/assets/images/elevation_cropland_map.png)

## 📝 Basic Usage Examples

### Rainfall Data (CHIRPS)
```R
df <- geolink_chirps(
    time_unit = "month",
    start_date = "2020-01-01",
    end_date = "2020-03-01",
    shp_dt = shp_dt[shp_dt$ADM1_PCODE == "NG001",],
    grid_size = 1000
)
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
# Error: "Unsupported file format. Please provide .dta file"

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

Project Link: [https://github.com/SSA-Statistical-Team-Projects/GeoLink](https://github.com/SSA-Statistical-Team-Projects/GeoLink)

---
Made with ❤️ by [The World Bank Group & The University of Southampton]
