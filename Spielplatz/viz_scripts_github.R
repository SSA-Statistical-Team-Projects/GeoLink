library(ggplot2)
library(sf)
library(scales)

# Get rainfall data with higher resolution
rainfall_map <- geolink_chirps(
  time_unit = "month",
  start_date = "2020-01-01",
  end_date = "2020-01-31",
  shp_dt = shp_dt[shp_dt$ADM1_EN == "Abia",],
  grid_size = 1000,
  extract_fun = "mean"
)

# Get Abia state boundary
abia_boundary <- shp_dt[shp_dt$ADM1_EN == "Abia",]

# Transform to WGS84 if needed
rainfall_map <- st_transform(rainfall_map, crs = 4326)
abia_boundary <- st_transform(abia_boundary, crs = 4326)

# Create detailed brown-to-blue color palette
detailed_colors <- c(
  "#8B4513",  # Dark brown (very low rainfall)
  "#A0522D",  # Sienna brown
  "#CD853F",  # Peru brown
  "#D2691E",  # Chocolate
  "#DAA520",  # Goldenrod
  "#F0E68C",  # Khaki (transition)
  "#F5F5DC",  # Beige
  "#E0FFFF",  # Light cyan
  "#B0E0E6",  # Powder blue
  "#87CEEB",  # Sky blue
  "#4682B4",  # Steel blue
  "#1E90FF",  # Dodger blue
  "#0000FF"   # Pure blue (very high rainfall)
)

# Create professional rainfall map with detailed color scheme
p <- ggplot(rainfall_map) +
  geom_sf(aes(fill = rainfall_month1), color = NA) +
  scale_fill_gradientn(
    name = "Rainfall\n(mm)",
    colors = detailed_colors,
    trans = "sqrt",
    labels = label_number(accuracy = 1),
    na.value = "transparent",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 18,
      barheight = 1,
      frame.colour = "gray30",
      ticks.colour = "gray30",
      nbin = 100  # Smooth color transitions
    )
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    # Add subtle grid for geographic reference
    panel.grid = element_line(color = "gray90", linewidth = 0.2),
    panel.background = element_rect(fill = "gray98", color = NA),

    # Text styling
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 14,
      hjust = 0.5,
      color = "grey30",
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 10,
      hjust = 1,
      color = "grey50",
      margin = margin(t = 15)
    ),

    # Legend styling
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.margin = margin(t = 20),

    # Overall plot styling
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20),

    # Axis styling
    axis.text = element_text(size = 8, color = "gray50"),
    axis.title = element_blank()
  ) +
  labs(
    title = "Rainfall Distribution - January 2020",
    subtitle = "Abia State, Nigeria",
    caption = "Data Source: CHIRPS | Detailed Color Scale: Brown (Dry) → Blue (Wet)"
  )

# Print the map
print(p)

# Save high-resolution version
ggsave(
  filename = "abia_rainfall_detailed_colors.png",
  plot = p,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "white"
)

# Save as PDF for vector graphics
ggsave(
  filename = "abia_rainfall_detailed_colors.pdf",
  plot = p,
  width = 14,
  height = 12,
  device = "pdf"
)


################################################################################################################################################

library(ggplot2)
library(sf)
library(scales)

# Get night time lights data
ntl_map <- geolink_ntl(
  time_unit = "annual",
  start_date = "2020-01-01",
  end_date = "2020-12-31",
  shp_dt = shp_dt[shp_dt$ADM1_EN == "Lagos",],
  indicator = "average_masked",
  grid_size = 1000
)

# Get Lagos state boundary
lagos_boundary <- shp_dt[shp_dt$ADM1_EN == "Lagos",]

# Transform to WGS84 if needed
ntl_map <- st_transform(ntl_map, crs = 4326)
lagos_boundary <- st_transform(lagos_boundary, crs = 4326)

# Create detailed night time lights color palette
detailed_ntl_colors <- c(
  "#000080",  # Navy blue (very low light)
  "#191970",  # Midnight blue
  "#4169E1",  # Royal blue
  "#0000FF",  # Blue
  "#1E90FF",  # Dodger blue
  "#00BFFF",  # Deep sky blue
  "#87CEEB",  # Sky blue
  "#00FFFF",  # Cyan
  "#7FFFD4",  # Aquamarine
  "#98FB98",  # Pale green
  "#FFFF00",  # Yellow
  "#FFD700",  # Gold
  "#FFA500",  # Orange
  "#FF4500"   # Orange red (very high light)
)

# Create professional night time lights map
p <- ggplot(ntl_map) +
  geom_sf(aes(fill = ntl_annual1average_masked), color = NA) +
  scale_fill_gradientn(
    name = "Light\nIntensity",
    colors = detailed_ntl_colors,
    trans = "sqrt",
    labels = label_number(accuracy = 0.1),
    na.value = "transparent",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 18,
      barheight = 1,
      frame.colour = "gray30",
      ticks.colour = "gray30",
      nbin = 100
    )
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    # White background styling
    panel.grid = element_line(color = "gray90", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),

    # Text styling
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      color = "black",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 14,
      hjust = 0.5,
      color = "grey30",
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 10,
      hjust = 1,
      color = "grey50",
      margin = margin(t = 15)
    ),

    # Legend styling
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.margin = margin(t = 20),

    # Overall plot styling
    plot.margin = margin(20, 20, 20, 20),

    # Axis styling
    axis.text = element_text(size = 8, color = "gray50"),
    axis.title = element_blank()
  ) +
  labs(
    title = "Night Time Light Intensity - 2020",
    subtitle = "Lagos State, Nigeria",
    caption = "Data Source: NASA Black Marble | Dark Blue (Low) → Yellow/Orange (High)"
  )

# Print the map
print(p)

# Save high-resolution version
ggsave(
  filename = "lagos_ntl_2020_white_background.png",
  plot = p,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "white"
)

# Save as PDF for vector graphics
ggsave(
  filename = "lagos_ntl_2020_white_background.pdf",
  plot = p,
  width = 14,
  height = 12,
  device = "pdf"
)

#######################################################################################################################################################


library(ggplot2)
library(sf)
library(scales)

# Get population data
pop_data <- geolink_population(
  start_year = 2018,
  end_year = 2018,
  iso_code = "NGA",
  constrained = 'N',
  UN_adjst = 'N',
  shp_dt = shp_dt[shp_dt$ADM1_EN == "Kano",],
  grid_size = 1000,
  extract_fun = "mean"
)

# Get Kano state boundary
kano_boundary <- shp_dt[shp_dt$ADM1_EN == "Kano",]

# Transform to WGS84 if needed
pop_data <- st_transform(pop_data, crs = 4326)
kano_boundary <- st_transform(kano_boundary, crs = 4326)

# Create detailed population density color palette (Yellow-Orange-Red spectrum)
detailed_pop_colors <- c(
  "#FFFFCC",  # Very light yellow (very low population)
  "#FFEDA0",  # Light yellow
  "#FED976",  # Yellow
  "#FEB24C",  # Light orange
  "#FD8D3C",  # Orange
  "#FC4E2A",  # Orange red
  "#E31A1C",  # Red
  "#BD0026",  # Dark red
  "#800026",  # Very dark red (very high population)
  "#4D0013"   # Deep maroon (extremely high population)
)

# Create professional population density map
p <- ggplot(pop_data) +
  geom_sf(aes(fill = population_2018), color = NA) +
  scale_fill_gradientn(
    name = "Population\nDensity",
    colors = detailed_pop_colors,
    trans = "sqrt",
    labels = label_number(accuracy = 1, scale = 1, suffix = ""),
    na.value = "transparent",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 18,
      barheight = 1,
      frame.colour = "gray30",
      ticks.colour = "gray30",
      nbin = 100
    )
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    # White background styling
    panel.grid = element_line(color = "gray90", linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),

    # Text styling
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      color = "black",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 14,
      hjust = 0.5,
      color = "grey30",
      margin = margin(b = 15)
    ),
    plot.caption = element_text(
      size = 10,
      hjust = 1,
      color = "grey50",
      margin = margin(t = 15)
    ),

    # Legend styling
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.margin = margin(t = 20),

    # Overall plot styling
    plot.margin = margin(20, 20, 20, 20),

    # Axis styling
    axis.text = element_text(size = 8, color = "gray50"),
    axis.title = element_blank()
  ) +
  labs(
    title = "Population Density Distribution - 2018",
    subtitle = "Kano State, Nigeria",
    caption = "Data Source: WorldPop | Light Yellow (Low) → Dark Red (High)"
  )

# Print the map
print(p)

# Save high-resolution version
ggsave(
  filename = "kano_population_2018_white_background.png",
  plot = p,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "white"
)

# Save as PDF for vector graphics
ggsave(
  filename = "kano_population_2018_white_background.pdf",
  plot = p,
  width = 14,
  height = 12,
  device = "pdf"
)

#########################################################################################################################################################
