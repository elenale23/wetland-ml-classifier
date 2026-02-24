library(terra)
library(sf)
library(whitebox)
library(here)

# DEM Processing Script: Reproject, resample, and derive DEM metrics (Slope, TWI, TPI, dist to stream). Then plot all metrics at the end
# 12 metrics in total: Elevation, Slope, TWI, TPI20, TPI100, pDep, Distance to Streams, B1 (Blue), B2 (Green), B3 (Red), B4 (NIR)

cat("Pre-run script update checklist: \n")
cat("✅ Load raster for each site \n")
cat("✅ Crop to new extents if necessary \n")
cat("✅ Update site_name and date \n")
cat("✅ Update output directory \n")
cat("✅ Update DEM at dem_original \n")
cat("✅ Update ps_raster \n")

## ---- Configuration ---- 

# Set site name and date here
site_name <- "pyramid"  # UPDATE depending on site
date <- "20250704" # UPDATE depending on date

# Output directory
output_dir <- here("outputs/processed_dem/pyramid/20250704") # UPDATE depending on site
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat("Processing site and date:", site_name, "-" ,date, "\n")

## ---- STEP 1: Load and Reproject DEM ----

# Load original DEM
dem_original <- rast("data/DEM/WorldDEM_DTM_04_N52_00_W120_00/DEM/WorldDEM_DTM_04_N52_00_W120_00_DEM.tif") # UPDATE depending on site
cat("Original DEM:\n")
cat("  Resolution:", res(dem_original)[1], "m\n")
cat("  CRS:", crs(dem_original, describe = TRUE)$name, "\n")

# Load PlanetScope for target specs
ps_raster <- rast("data/cropped_wetland_rasters/pyramid/pyramid_20250704.tif") # UPDATE depending on site
target_crs <- crs(ps_raster)
target_extent <- ext(ps_raster)
target_res <- res(ps_raster)

# Make sure that bands are named correctly
names(ps_raster)
names(ps_raster) <- c("blue", "green", "red", "nir")
plot(ps_raster)

cat(" Target (PlanetScope):\n")
cat("  Resolution:", target_res[1], "m\n")
cat("  CRS:", crs(ps_raster, describe = TRUE)$name, "\n")

# Reproject DEM to match PlanetScope's CRS
dem_reprojected <- project(dem_original, target_crs, method = "bilinear")

# Resample to 3m to match PlanetScope's resolution
dem_resampled <- resample(dem_reprojected, ps_raster)

# Clip to extent
dem_clipped <- crop(dem_resampled, target_extent)

# Save processed DEM
dem_3m_path <- here(output_dir, paste0(site_name, "_", date, "_dem_3m.tif"))
writeRaster(dem_clipped, dem_3m_path, overwrite = TRUE)

# Visualize DEM
plot(dem_clipped, main = paste("Elevation -", site_name, "-", date), col = terrain.colors(100))

# Save plot to file
png(here(output_dir, paste0(site_name, "_", date, "_elevation.png")), 
    width = 2400, height = 2000, res = 300)
plot(dem_clipped, main = paste("Elevation -", site_name, "-", date), col = terrain.colors(100))
dev.off()

## ---- STEP 2: Calculate Slope ----

slope_path <- here(output_dir, paste0(site_name, "_", date, "_slope_3m.tif"))
if (file.exists(slope_path)) file.remove(slope_path)

wbt_slope(
  dem = dem_3m_path,
  output = slope_path,
  units = "degrees"
)

# Load and visualize
slope <- rast(slope_path)
cat("\nSlope statistics:\n")
print(summary(values(slope)))

# Visualize
plot(slope, main = paste("Slope -", site_name, "-", date), col = terrain.colors(100))

# How many zero-slope cells?
zero_slope <- global(slope, function(x) sum(x == 0, na.rm = TRUE))[[1]]
print(zero_slope)

# Save plot to file
png(here(output_dir, paste0(site_name, "_", date, "_slope.png")), 
    width = 2400, height = 2000, res = 300)
plot(slope, main = paste("Slope -", site_name, "-", date), col = terrain.colors(100))
dev.off()

## ---- STEP 3: Calculate TWI ----
# Topographic wetness index (TWI): the tendency of an area to accumulate water – aka how likely it is to be wet. TWI=ln(SCA/tanφ), where SCA is Specific Catchment Area and φ is slope. 

# Step 3a: Fill depressions in the DEM
# To calculate SCA, first, we need to fill in depressions in the DEM using the BreachDepressionsLeastCost tool 
# The dist parameter is defined as maximum search distance for breach paths in cells. We will try dist = 10 cells (30 m) 

dem_breached_path <- here(output_dir, paste0(site_name, "_", date,"_dem_breached_dist", 
                                             10, ".tif"))
if (file.exists(dem_breached_path)) file.remove(dem_breached_path)

wbt_breach_depressions_least_cost(
  dem = dem_3m_path,
  output = dem_breached_path,
  dist = 10,
  fill = TRUE
)

# Calculate difference from original
dem_breached <- rast(dem_breached_path)
breach_diff <- dem_breached - dem_clipped  # Where breaching occurred

# Step 3b: Calculate Specific Contributing Area

sca_path <- here(output_dir, paste0(site_name, "_", date, "_sca.tif"))
if (file.exists(sca_path)) file.remove(sca_path)

# Calculate SCA
wbt_d_inf_flow_accumulation(
  input = dem_breached,
  output = sca_path,
  out_type = "Specific Contributing Area"
)

# Step 3c: Calculate TWI

twi_path <- here(output_dir, paste0(site_name, "_", date, "_twi_dist", 10, ".tif"))
if (file.exists(twi_path)) file.remove(twi_path)

# Add a small constant to slope before calculating TWI in case there are zero slope values
slope_adjusted <- slope + 0.001
slope_adjusted_path <- writeRaster(slope_adjusted, here(output_dir, paste0(site_name, "_", date, "_slope_adjusted_3m.tif")))

# Calculate TWI
wbt_wetness_index(
  sca = sca_path,
  slope = slope_adjusted_path,
  output = twi_path
)

# Visualize TWI
twi <- rast(twi_path)
print(summary(values(twi)))

# Plot
plot(twi, main = paste("TWI (dist =", 10, ") -", site_name, "-", date),
     col = terrain.colors(100), breaks = seq(0, 30, by = 2))

# Save
png(here(output_dir, paste0(site_name, "_", date, "_twi_dist10.png")), 
    width = 2400, height = 2000, res = 300)
plot(twi, main = paste("TWI (dist =", 10, ") -", site_name, "-", date),
     col = terrain.colors(100), breaks = seq(0, 30, by = 2))
dev.off()

## ---- STEP 4: Calculate Topographic Position Index. TPI compares elevation of each cell to the mean elevation of surrounding cells. ----
# After testing out a few, I will use two neighborhood sizes to capture fine-scale and large scale features: 20 x 20 cells (400 m) and  100 x 100 cells (1000 m) 

# Calculate TPI 20

tpi_path_20 <- here(output_dir, paste0(site_name, "_", date, "_tpi_", 20, ".tif"))
if (file.exists(tpi_path_20)) file.remove(tpi_path_20)

wbt_diff_from_mean_elev(
  dem = dem_3m_path,
  output = tpi_path_20,
  filterx = 20,
  filtery = 20
)

# Visualize TPI
tpi20 <- rast(tpi_path_20)
print(summary(values(tpi20)))

# Plot
plot(tpi20, main = paste("TPI (neighborbood size =", 20, ") -", site_name, "-", date),
     col = terrain.colors(100))

# Save
png(here(output_dir, paste0(site_name, "_", date, "_tpi_20.png")), 
    width = 2400, height = 2000, res = 300)
plot(tpi20, main = paste("TPI (neighborbood size =", 20, ") -", site_name, "-", date),
     col = terrain.colors(100))
dev.off()

# Calculate TPI 100

tpi_path_100 <- here(output_dir, paste0(site_name, "_", date, "_tpi_", 100, ".tif"))
if (file.exists(tpi_path_100)) file.remove(tpi_path_100)

wbt_diff_from_mean_elev(
  dem = dem_3m_path,
  output = tpi_path_100,
  filterx = 100,
  filtery = 100
)

# Visualize TPI
tpi100 <- rast(tpi_path_100)
print(summary(values(tpi100)))

# Plot
plot(tpi100, main = paste("TPI (neighborbood size =", 100, ") -", site_name, "-", date),
     col = terrain.colors(100))

# Save
png(here(output_dir, paste0(site_name, "_", date, "_tpi_100.png")), 
    width = 2400, height = 2000, res = 300)
plot(tpi100, main = paste("TPI (neighborbood size =", 100, ") -", site_name, "-", date),
     col = terrain.colors(100))
dev.off()

## ---- STEP 5: Extract Streams and Calculate Distance ----
# I'm going to use the BC Freshwater Atlas Stream Network provincial layer to calculate distance from streams

# Use BC Freshwater Atlas streams
streams_path <- here("data/FWA_stream_network/FWSTRMNTWR_line.shp")

# Load and process BC streams
streams <- read_sf(streams_path)
streams_utm <- st_transform(streams, crs = st_crs(ps_raster))

# Crop to site extent
site_extent_bbox <- st_bbox(c(xmin = xmin(target_extent) - 100,
                              xmax = xmax(target_extent) + 100,
                              ymin = ymin(target_extent) - 100,
                              ymax = ymax(target_extent) + 100),
                            crs = st_crs(streams_utm))

streams_cropped <- st_crop(streams_utm, site_extent_bbox)
cat("Cropped to", nrow(streams_cropped), "stream segments\n")

# Overlay on DEM to view
plot(dem_clipped, main = paste("Stream Network -", site_name, "_", date, "\n"),
     col = gray.colors(100))
plot(streams, col = "blue", add = TRUE, legend = FALSE)

# Save as temporary shapefile
streams_temp_shp <- here(output_dir, paste0(site_name, "_", date, "_streams_temp.shp"))
st_write(streams_cropped, streams_temp_shp, delete_dsn = TRUE, quiet = TRUE)

# Rasterize streams shapefile
streams_raster_path <- here(output_dir, paste0(site_name, "_", date, "_streams.tif"))
if (file.exists(streams_raster_path)) file.remove(streams_raster_path)

wbt_vector_lines_to_raster(
  input = streams_temp_shp,
  output = streams_raster_path,
  field = "FID",
  base = dem_3m_path
)

# Visualize streams
streams <- rast(streams_raster_path)

# Plot streams
plot(dem_clipped, main = paste("Stream Network -", site_name, "-", date, "\n"),
     col = gray.colors(100))
plot(streams, col = "blue", add = TRUE, legend = FALSE)

# Calculate euclidean distance to streams using use terra::distance()
dist_streams_path <- here(output_dir, paste0(site_name, "_", date, "_dist_streams.tif"))
if (file.exists(dist_streams_path)) file.remove(dist_streams_path)

# Check stream raster first
streams_check <- rast(streams_raster_path)
stream_cells <- global(streams_check, function(x) sum(x > 0, na.rm = TRUE))[[1]]
cat("Stream cells in raster:", stream_cells, "\n")

# Use terra::distance()
streams_vect <- as.polygons(streams_check > 0, dissolve = TRUE)
dist_streams <- distance(dem_clipped, streams_vect)
writeRaster(dist_streams, dist_streams_path, overwrite = TRUE)

# Visualize distance to streams
dist_streams <- rast(dist_streams_path)
print(summary(values(dist_streams)))

# Plot
plot(dist_streams, main = paste("Distance to Streams (m) -", site_name, "-", date, "\n"),
     col = terrain.colors(100))

# Save
png(here(output_dir, paste0(site_name, "_", date, "_dist_streams.png")), 
    width = 2400, height = 2000, res = 300)
plot(dist_streams, main = paste("Distance to Streams (m) -", site_name, "-", date, "\n"),
     col = rev(heat.colors(100)))
dev.off()

# Clean up temporary shapefile and associated files
cat("Cleaning up temporary files...\n")
if (file.exists(streams_temp_shp)) {
  # Shapefiles have multiple associated files (.shp, .shx, .dbf, .prj, etc.)
  temp_pattern <- tools::file_path_sans_ext(basename(streams_temp_shp))
  temp_files <- list.files(
    path = dirname(streams_temp_shp),
    pattern = paste0("^", temp_pattern, "\\."),
    full.names = TRUE
  )
  suppressWarnings(file.remove(temp_files))
}

## ---- STEP 6: Calculate Stochastic Depression Probability ----

# Pdep (probability of depression) - accounts for DEM uncertainty
# Higher pdep = more likely to be a depression/wetland

# Parameters for error model
rmse_value <- 1     # DEM RMSE in meters 
range_value <- 10      # Error autocorrelation length in meters. Rule of thumb is 3-15 times DEM's resolution
iterations <- 300      # Number of iterations (more = smoother, slower)

pdep_path <- here(output_dir, paste0(site_name, "_", date, "_pdep.tif"))
if (file.exists(pdep_path)) file.remove(pdep_path)

# Run stochastic depression analysis
wbt_stochastic_depression_analysis(
  dem = dem_3m_path,  # Use original DEM, not breached
  output = pdep_path,
  rmse = rmse_value,
  range = range_value,
  iterations = iterations
)

# Load and visualize
pdep <- rast(pdep_path)
print(summary(values(pdep)))

cat("\nPdep interpretation:\n")
cat("  0.0-0.2 = Very unlikely to be depression \n")
cat("  0.2-0.4 = Low probability \n")
cat("  0.4-0.6 = Moderate probability \n")
cat("  0.6-0.8 = High probability \n")
cat("  0.8-1.0 = Very high probability\n")

# Create bins for visualization
breaks_pdep <- seq(0, 1, by = 0.2)
colors_pdep <- colorRampPalette(c("brown", "yellow", "lightblue", "blue", "darkblue"))(5)

# Plot
plot(pdep, 
     main = paste("Depression Probability (pdep) -", site_name, "-", date,
                  "\nRMSE =", rmse_value, "m, Range =", range_value, "m,", iterations, "iterations"),
     col = colors_pdep,
     breaks = breaks_pdep,
     plg = list(title = "Probability"))

# Save
png(here(output_dir, paste0(site_name, "_", date, "_pdep.png")), 
    width = 2400, height = 2000, res = 300)
plot(pdep, 
     main = paste("Depression Probability (pdep) -", site_name,"-", date,
                  "\nRMSE =", rmse_value, "m, Range =", range_value, "m,", iterations, "iterations"),
     col = colors_pdep,
     breaks = breaks_pdep,
     plg = list(title = "Probability"))

dev.off()

## ---- STEP 7: Stack All Features ----

# Stack DEM-derived features
dem_features <- c(dem_clipped, slope, twi, tpi20, tpi100, pdep, dist_streams)
names(dem_features) <- c("elevation", "slope", "twi", "tpi20", "tpi100", "pdep", "dist_streams")

# Stack spectral features
blue <- ps_raster$blue
green <- ps_raster$green
red <- ps_raster$red
nir <- ps_raster$nir

spectral_features <- c(blue, green, red, nir)

# Combine all features into one stack
all_features <- c(dem_features, spectral_features)

plot(all_features)

cat("Total features stacked:", nlyr(all_features), "\n")
cat("Feature names:", paste(names(all_features), collapse = ", "), "\n")

# Check for consistent extent and resolution
cat("  Resolution:", paste(res(all_features)[1:2], collapse = " x "), "m\n")
cat("  Dimensions:", paste(dim(all_features)[1:2], collapse = " x "), "cells\n")

# Save stacked features
stacked_path <- here(output_dir, paste0(site_name, "_", date, "_stacked_features.tif"))
writeRaster(all_features, stacked_path, overwrite = TRUE)
head(all_features)

# Check for NAs in final stack
na_summary_stack <- data.frame(
  Feature = names(all_features),
  NA_count = sapply(1:nlyr(all_features), function(i) {
    global(all_features[[i]], function(x) sum(is.na(x)))[[1]]
  }),
  NA_percent = sapply(1:nlyr(all_features), function(i) {
    global(all_features[[i]], function(x) sum(is.na(x)) / length(x) * 100)[[1]]
  })
)

cat("\nNA summary for stacked features:\n")
print(na_summary_stack)

cat("\n========================================\n")
cat("Processing Complete!\n")
cat("========================================\n")
cat("Check outputs in:", output_dir, "\n")
