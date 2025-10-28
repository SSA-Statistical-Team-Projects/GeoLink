library(GeoLink)
library(mapview)
library(sf)
library(viridis)
library(leaflet)
library(leafpop)
library(htmlwidgets)

# Check current working directory
cat("Current working directory:", getwd(), "\n")

# Create the full path for the output directory
output_dir <- file.path(getwd(), "docs", "interactive-examples")

# Create directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created directory:", output_dir, "\n")
} else {
  cat("Directory already exists:", output_dir, "\n")
}

# ============================================================================
# 1. RAINFALL VISUALIZATION - INTERACTIVE VERSION
# ============================================================================

# Get rainfall data
rainfall_map <- geolink_chirps(
  time_unit = "month",
  start_date = "2020-01-01",
  end_date = "2020-01-31",
  shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
  grid_size = 1000
)

# Create interactive rainfall map with mapview
rainfall_interactive <- mapview(
  rainfall_map,
  zcol = "rainfall_month1",
  col.regions = plasma(100),
  alpha.regions = 0.8,
  layer.name = "Rainfall (mm)",
  label = paste0(round(rainfall_map$rainfall_month1, 1), " mm"),
  popup = leafpop::popupTable(
    rainfall_map,
    zcol = c("rainfall_month1"),
    feature.id = FALSE,
    row.numbers = FALSE
  ),
  map.types = c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap"),
  legend = TRUE,
  homebutton = TRUE
)

# Add title and additional controls
rainfall_interactive@map <- rainfall_interactive@map %>%
  addControl(
    html = "<h4 style='padding:10px; background:white; border-radius:5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>
                January 2020 Rainfall Distribution<br>
                <small style='color:#666;'>Abia State, Nigeria | Data: CHIRPS</small></h4>",
    position = "topright"
  ) %>%
  addScaleBar(position = "bottomright")

# Save with FULL PATH
rainfall_output <- file.path(output_dir, "rainfall_map.html")
htmlwidgets::saveWidget(
  widget = rainfall_interactive@map,
  file = rainfall_output,
  selfcontained = TRUE,
  title = "Rainfall Map - Abia State | GeoLink"
)

# Check if file was created
if (file.exists(rainfall_output)) {
  cat("✓ Rainfall map saved successfully to:", rainfall_output, "\n")
} else {
  cat("✗ Failed to save rainfall map\n")
}


# ============================================================================
# 3. POPULATION DENSITY - INTERACTIVE VERSION
# ============================================================================
# Get population data
pop_data <- geolink_population(
  start_year = 2018,
  end_year = 2018,
  iso_code = "NGA",
  constrained = 'N',
  UN_adjst = 'N',
  shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
  grid_size = 1000,
  extract_fun = "mean"
)

# Calculate population density (per km²)
# Convert area from m² to km²
pop_data$area_km2 <- as.numeric(st_area(pop_data)) / 1000000
pop_data$pop_density_km2 <- pop_data$population_2018 / pop_data$area_km2

# Standardize density to 0-100 scale
min_density <- min(pop_data$pop_density_km2, na.rm = TRUE)
max_density <- max(pop_data$pop_density_km2, na.rm = TRUE)

# Standardize using min-max normalization to 0-100
pop_data$density_standardized <- ((pop_data$pop_density_km2 - min_density) / (max_density - min_density)) * 100

# Print summary statistics
cat("Population Density Statistics:\n")
cat("  Min density:", round(min_density, 2), "people/km²\n")
cat("  Max density:", round(max_density, 2), "people/km²\n")
cat("  Mean density:", round(mean(pop_data$pop_density_km2, na.rm = TRUE), 2), "people/km²\n")

# Create interactive population density map
pop_interactive <- mapview(
  pop_data,
  zcol = "density_standardized",
  col.regions = colorRampPalette(c(
    "#ffffcc",  # Light yellow for low density
    "#ffeda0",
    "#fed976",
    "#feb24c",
    "#fd8d3c",
    "#fc4e2a",
    "#e31a1c",
    "#b10026"   # Dark red for high density
  ))(100),
  alpha.regions = 0.8,
  layer.name = "Pop. Density<br>(Standardized)",
  label = paste0(
    "Standardized: ", round(pop_data$density_standardized, 1),
    " | Actual: ", round(pop_data$pop_density_km2, 0), " per km²"
  ),
  popup = leafpop::popupTable(
    data.frame(
      "Population (2018)" = format(round(pop_data$population_2018, 0), big.mark = ","),
      "Area (km²)" = round(pop_data$area_km2, 2),
      "Density (per km²)" = round(pop_data$pop_density_km2, 1),
      "Standardized (0-100)" = round(pop_data$density_standardized, 1),
      check.names = FALSE
    ),
    feature.id = FALSE,
    row.numbers = FALSE
  ),
  map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "OpenStreetMap"),
  legend = TRUE,
  homebutton = TRUE
)

# Add controls and custom legend
pop_interactive@map <- pop_interactive@map %>%
  addControl(
    html = paste0(
      "<h4 style='padding:10px; background:white; border-radius:5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>",
      "Population Density 2018 (Standardized)<br>",
      "<small style='color:#666;'>Abia State, Nigeria | Data: WorldPop</small><br>",
      "<small style='color:#999;'>Range: ",
      round(min_density, 0), " - ",
      round(max_density, 0), " people/km²</small>",
      "</h4>"
    ),
    position = "topright"
  ) %>%
  addScaleBar(position = "bottomright")

# Save with FULL PATH
pop_output <- file.path(output_dir, "population_density_map.html")
htmlwidgets::saveWidget(
  widget = pop_interactive@map,
  file = pop_output,
  selfcontained = TRUE,
  title = "Population Density (Standardized) - Abia State | GeoLink"
)

if (file.exists(pop_output)) {
  cat("✓ Population density map saved successfully to:", pop_output, "\n")
} else {
  cat("✗ Failed to save population density map\n")
}
