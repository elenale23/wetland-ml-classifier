library(sf)
library(terra)
library(here)
library(dplyr)

## ---- Crop rasters as closely to the wetland extents as possible ---- ##
cook_raster <- rast("data/imagery/cook_jul_25_psscene_analytic_sr_udm2/PSScene/composite.tif")
pyramid_raster <- rast("data/imagery/pyramid_jul_4_2025_psscene_analytic_sr_udm2/composite.tif")
tumtum_raster <- rast("data/imagery/tumtum_20250715_psscene_analytic_sr_udm2/composite.tif")

plotRGB(pyramid_raster, r = 3, g = 2, b = 1, stretch = "lin") # visualize the raster 

# Apply new raster extents 
cook_new_extent = ext(344000, 345100, 5778800, 5781200)
cook_raster_new_extent <- crop(cook_raster, cook_new_extent)
plotRGB(cook_raster_new_extent, r = 3, g = 2, b = 1, stretch = "lin") # visualize cropped raster
cook_raster_cropped <- writeRaster(cook_raster_new_extent, here("data/cropped_wetland_rasters/cook_20250704.tif"), overwrite = TRUE)
cook_raster_new_extent <- as.polygons(ext(cook_raster_cropped), crs = crs(cook_raster_cropped))
writeVector(cook_raster_new_extent, here("data/raster_extents/cook_raster_extent.shp"), overwrite = TRUE)

pyramid_new_extent = ext(351460, 351640, 5803400, 5803800)
pyramid_raster_new_extent <- crop(pyramid_raster, pyramid_new_extent)
plotRGB(pyramid_raster_new_extent, r = 3, g = 2, b = 1, stretch = "lin")
pyramid_raster_cropped <- writeRaster(pyramid_raster_new_extent, here("data/cropped_wetland_rasters/pyramid_20250704.tif"), overwrite = TRUE)
pyramid_raster_new_extent <- as.polygons(ext(pyramid_raster_cropped), crs = crs(pyramid_raster_cropped))
writeVector(pyramid_raster_new_extent, here("data/raster_extents/pyramid_raster_extent.shp"), overwrite = TRUE)

tumtum_new_extent = ext(342010, 342530, 5743450, 5745020)
tumtum_raster_new_extent <- crop(tumtum_raster, tumtum_new_extent)
plotRGB(tumtum_raster_new_extent, r = 3, g = 2, b = 1, stretch = "lin")
tumtum_raster_cropped <- writeRaster(tumtum_raster_new_extent, here("data/cropped_wetland_rasters/tumtum_20250715.tif"), overwrite = TRUE)
tumtum_raster_new_extent <- as.polygons(ext(tumtum_raster_cropped), crs = crs(tumtum_raster_cropped))
writeVector(tumtum_raster_new_extent, here("data/raster_extents/tumtum_raster_extent.shp"), overwrite = TRUE)

# Filter Training Points to Match New Raster Extents
# Removes points that fall outside the cropped raster boundaries

## ---- Configuration ----
# Path to your training points
training_points_path <- here("outputs/cleaned_data/all_points.rds")

# Output path for filtered points
output_path <- here("outputs/cleaned_data/filtered_points.rds")

# Sites to process
sites <- c("cook", "pyramid", "tumtum")

## ---- Load Training Points ----
cat("Loading training points...\n")
all_points <- readRDS(training_points_path)

cat("Original points:", nrow(all_points), "\n")
cat("Sites:", paste(unique(all_points$site), collapse = ", "), "\n")
cat("Classes:", paste(unique(all_points$class), collapse = ", "), "\n")

# Summary by site and class
cat("\nOriginal distribution:\n")
print(table(all_points$site, all_points$class))

## ---- Filter Points by Raster Extent Shapefile ----
filtered_points <- data.frame()

for (site in sites) {
  cat("\n=== Processing", site, "===\n")
  
  # Get site points
  site_points <- all_points[all_points$site == site, ]
  cat("  Original points:", nrow(site_points), "\n")
  
  # Load the extent shapefile for this site
  extent_shp_path <- here("data/raster_extents", paste0(site, "_raster_extent.shp"))
  
  if (!file.exists(extent_shp_path)) {
    cat("  ⚠️  No extent shapefile found for", site, "at:", extent_shp_path, "\n")
    cat("  Skipping this site...\n")
    next
  }
  
  cat("  Using extent shapefile:", basename(extent_shp_path), "\n")
  
  # Load extent polygon
  extent_polygon <- st_read(extent_shp_path, quiet = TRUE)
  extent_crs <- st_crs(extent_polygon)
  
  cat("  Extent CRS:", st_crs(extent_polygon)$input, "\n")
  
  # Print extent bounds
  bbox <- st_bbox(extent_polygon)
  cat("  Extent bounds:\n")
  cat("    xmin:", bbox["xmin"], "xmax:", bbox["xmax"], "\n")
  cat("    ymin:", bbox["ymin"], "ymax:", bbox["ymax"], "\n")
  
  # Convert points to sf (if not already)
  # Check for different possible coordinate column names
  coord_cols <- NULL
  
  if ("x_coord" %in% names(site_points) && "y_coord" %in% names(site_points)) {
    coord_cols <- c("x_coord", "y_coord")
  } else if ("easting" %in% names(site_points) && "northing" %in% names(site_points)) {
    coord_cols <- c("easting", "northing")
  } 
  
  if (is.null(coord_cols)) {
    cat("  ⚠️  No coordinate columns found. Available columns:\n")
    cat("     ", paste(names(site_points), collapse = ", "), "\n")
    next
  }
  
  cat("  Using coordinate columns:", paste(coord_cols, collapse = ", "), "\n")
  
  # Convert points to sf with the same CRS as extent polygon
  site_points_sf <- st_as_sf(site_points, 
                             coords = coord_cols,
                             crs = extent_crs)
  
  # Filter points inside extent polygon
  points_inside <- st_intersects(site_points_sf, extent_polygon, sparse = FALSE)
  site_points_filtered_sf <- site_points_sf[points_inside[,1], ]
  
  # Convert back to dataframe with coordinates
  coords <- st_coordinates(site_points_filtered_sf)
  site_points_filtered <- st_drop_geometry(site_points_filtered_sf)
  
  # Restore original coordinate column names
  site_points_filtered[[coord_cols[1]]] <- coords[,1]
  site_points_filtered[[coord_cols[2]]] <- coords[,2]
  
  # Report results
  n_removed <- nrow(site_points) - nrow(site_points_filtered)
  cat("  Points inside extent:", nrow(site_points_filtered), "\n")
  cat("  Points removed:", n_removed, "\n")
  
  if (n_removed > 0) {
    cat("  Removed by class:\n")
    removed_points <- site_points[!points_inside[,1], ]
    print(table(removed_points$class))
  }
  
  # Add to filtered collection
  filtered_points <- rbind(filtered_points, site_points_filtered)
}

## ---- Summary of Filtered Dataset ----
cat("\n=== SUMMARY ===\n")
cat("Original total points:", nrow(all_points), "\n")
cat("Filtered total points:", nrow(filtered_points), "\n")
cat("Points removed:", nrow(all_points) - nrow(filtered_points), "\n")

if (nrow(all_points) > 0) {
  cat("Removal rate:", round((nrow(all_points) - nrow(filtered_points)) / nrow(all_points) * 100, 1), "%\n")
}

cat("\nFiltered distribution:\n")
print(table(filtered_points$site, filtered_points$class))

cat("\nPoints removed by site:\n")
removal_by_site <- data.frame(
  Site = sites,
  Original = sapply(sites, function(s) sum(all_points$site == s)),
  Filtered = sapply(sites, function(s) sum(filtered_points$site == s)),
  Removed = sapply(sites, function(s) sum(all_points$site == s) - sum(filtered_points$site == s))
)
print(removal_by_site)

## ---- Check for Class Imbalance ----
cat("\nChecking for class imbalance issues...\n")
class_counts <- table(filtered_points$class)
min_class <- min(class_counts)
max_class <- max(class_counts)
imbalance_ratio <- max_class / min_class

cat("Class counts:\n")
print(class_counts)
cat("\nImbalance ratio (max/min):", round(imbalance_ratio, 2), "\n")

if (imbalance_ratio > 3) {
  cat("⚠️  WARNING: Significant class imbalance detected!\n")
  cat("   Consider collecting more points for:", names(which.min(class_counts)), "\n")
} else {
  cat("✅ Class balance is acceptable\n")
}

## ---- Save Filtered Points ----
cat("\nSaving filtered points...\n")
saveRDS(filtered_points, output_path)
cat("✅ Saved to:", output_path, "\n")

# Also save as shapefile for visualization
# Detect coordinate columns in filtered data
final_coord_cols <- NULL
if ("easting" %in% names(filtered_points)) {
  final_coord_cols <- c("easting", "northing")
} else if ("x_coord" %in% names(filtered_points)) {
  final_coord_cols <- c("x_coord", "y_coord")
} else if ("x" %in% names(filtered_points)) {
  final_coord_cols <- c("x", "y")
}

if (!is.null(final_coord_cols)) {
  output_shp <- here("outputs/cleaned_data/filtered_points.shp")
  
  # Get CRS from last extent polygon loaded
  last_extent <- st_read(here("data/raster_extents", paste0(sites[length(sites)], "_raster_extent.shp")), quiet = TRUE)
  extent_crs <- st_crs(last_extent)
  
  filtered_points_sf <- st_as_sf(filtered_points, 
                                 coords = final_coord_cols,
                                 crs = extent_crs)
  st_write(filtered_points_sf, output_shp, delete_dsn = TRUE, quiet = TRUE)
  cat("✅ Saved shapefile to:", output_shp, "\n")
}

cat("\n========================================\n")
cat("Point Filtering Complete!\n")
cat("========================================\n")

## ---- Plot all three sites side by side ----

# Load the cropped wetland rasters

r_cook <- rast("data/cropped_wetland_rasters/cook_20250704.tif")
r_pyramid <- rast("data/cropped_wetland_rasters/pyramid_20250704.tif")
r_tumtum <- rast("data/cropped_wetland_rasters/tumtum_20250715.tif")
  
# Colors
class_colors <- c(
  "open_water" = "cyan",
  "wetland" = "limegreen",
  "upland" = "orange"
)

# Set up 3-panel plot
par(mfrow = c(1, 3), mar = c(2, 2, 3, 1))

# Cook
plotRGB(r_cook, r = 3, g = 2, b = 1, stretch = "lin", main = "COOK")
train_cook <- filtered_points_sf[filtered_points_sf$site == "cook", ]
for (class_name in unique(train_cook$class)) {
  points_class <- train_cook[train_cook$class == class_name, ]
  plot(st_geometry(points_class), add = TRUE, pch = 16, cex = 1, 
       col = class_colors[class_name])
}

# Tumtum
plotRGB(r_tumtum, r = 3, g = 2, b = 1, stretch = "lin", main = "TUMTUM")
train_tumtum <- filtered_points_sf[filtered_points_sf$site == "tumtum", ]
for (class_name in unique(train_tumtum$class)) {
  points_class <- train_tumtum[train_tumtum$class == class_name, ]
  plot(st_geometry(points_class), add = TRUE, pch = 16, cex = 1, 
       col = class_colors[class_name])
}

# Pyramid
plotRGB(r_pyramid, r = 3, g = 2, b = 1, stretch = "lin", main = "PYRAMID")
train_pyramid <- filtered_points_sf[filtered_points_sf$site == "pyramid", ]
for (class_name in unique(train_pyramid$class)) {
  points_class <- train_pyramid[train_pyramid$class == class_name, ]
  plot(st_geometry(points_class), add = TRUE, pch = 16, cex = 1, 
       col = class_colors[class_name])
}

dev.off()
