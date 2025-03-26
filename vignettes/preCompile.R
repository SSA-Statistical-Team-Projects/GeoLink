# Pre-compiled vignettes that take a significant amount of time and memory to run
# Must manually move image files from GeoLink/ to GeoLink/vignettes/ after knit
knitr::knit("vignettes/zonal_statistics_example_nigeria.qmd.orig", "vignettes/zonal_statistics_example_nigeria.qmd")
fs::dir_copy(path = "figure", new_path = "vignettes/figure")
fs::dir_delete("figure")
