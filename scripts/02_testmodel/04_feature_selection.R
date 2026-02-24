library(terra)
library(sf)
library(here)
library(mlr3)
library(mlr3filters)
library(corrplot)
library(dplyr)

# Complete Feature Selection Workflow
# Combines data from all sites, then performs correlation and filter analysis

## ---- Configuration ----
sites <- c("cook", "pyramid", "tumtum")
output_dir <- here("outputs/feature_selection")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

## ---- STEP 1: Extract Training Data from All Sites ----
cat("=== STEP 1: Extracting Training Data from All Sites ===\n")

## ---- Extract values while preserving original coordinates ----

all_training_data <- data.frame()

for (site in sites) {
  cat("\n=== Processing", site, "===\n")
  
  # Load predictor stack
  stack_path <- here("data/dem_processed", paste0(site, "_stacked_features.tif"))
  predictor_stack <- rast(stack_path)
  cat("  Loaded", nlyr(predictor_stack), "predictors\n")
  
  # Load training points
  points_path <- here("outputs/cleaned_data/filtered_points.shp")
  all_points_sf <- st_read(points_path, quiet = TRUE)
  
  # Filter to this site
  site_points_sf <- all_points_sf[all_points_sf$site == site, ]
  cat("  Found", nrow(site_points_sf), "training points\n")
  
  # SAVE THE ORIGINAL DATA (with easting/northing) BEFORE extraction
  original_data <- st_drop_geometry(site_points_sf)
  
  # Check if easting/northing exist in original data
  has_coords <- "easting" %in% names(original_data) && "northing" %in% names(original_data)
  
  if (!has_coords) {
    # If not, extract from geometry
    coords <- st_coordinates(site_points_sf)
    original_data$easting <- coords[, 1]
    original_data$northing <- coords[, 2]
  }
  
  cat("  Original columns:", paste(names(original_data), collapse = ", "), "\n")
  
  # Check CRS
  if (st_crs(site_points_sf) != st_crs(predictor_stack)) {
    site_points_sf <- st_transform(site_points_sf, st_crs(predictor_stack))
  }
  
  # Extract predictor values
  values <- terra::extract(predictor_stack, site_points_sf)
  
  # Combine: original data + extracted values (remove ID column from extract)
  site_data <- cbind(original_data, values[, -1, drop = FALSE])
  
  cat("  Final columns:", paste(names(site_data), collapse = ", "), "\n")
  cat("  ✅ Extracted values for", nrow(site_data), "points\n")
  
  # Combine with all data
  all_training_data <- bind_rows(all_training_data, site_data)
}

cat("\n✅ Total points:", nrow(all_training_data), "\n")
cat("Columns:", paste(names(all_training_data), collapse = ", "), "\n")


# Remove ID column from extract()
if ("ID" %in% names(all_training_data)) {
  all_training_data <- all_training_data[, -which(names(all_training_data) == "ID")]
}

cat("\n✅ Total training samples:", nrow(all_training_data), "\n")
cat("   Sites:", paste(unique(all_training_data$site), collapse = ", "), "\n")
cat("   Classes:", paste(unique(all_training_data$class), collapse = ", "), "\n")

# Before saving, ensure class is factor
all_training_data$class <- as.factor(all_training_data$class)

# Verify
cat("Saving data with class type:", class(all_training_data$class), "\n")
cat("Class levels:", paste(levels(all_training_data$class), collapse = ", "), "\n")

saveRDS(all_training_data, here("outputs/feature_selection/all_training_data.rds"))

## Save points with extracted features as a shapefile 
# Detect coordinate columns in filtered data
final_coord_cols <- NULL
if ("easting" %in% names(all_training_data)) {
  final_coord_cols <- c("easting", "northing")
} else if ("x_coord" %in% names(all_training_data)) {
  final_coord_cols <- c("x_coord", "y_coord")
} else if ("x" %in% names(all_training_data)) {
  final_coord_cols <- c("x", "y")
}

all_training_data_output <- here("outputs/cleaned_data/all_training_data.shp")

# Save training data as shp file if needed
last_extent <- st_read(here("data/raster_extents", paste0(sites[length(sites)], "_raster_extent.shp")), quiet = TRUE)
extent_crs <- st_crs(last_extent)
all_training_data_sf <- st_as_sf(all_training_data, 
                               coords = final_coord_cols,
                               crs = extent_crs)
st_write(all_training_data_sf, all_training_data_output, delete_dsn = TRUE, quiet = TRUE)

# Plot points on RGB for sanity check
# Set up 3-panel plot
par(mfrow = c(1, 3), mar = c(2, 2, 3, 1))

# Cook
plotRGB(r_cook, r = 3, g = 2, b = 1, stretch = "lin", main = "COOK")
train_cook_extracted <- all_training_data_sf[all_training_data_sf$site == "cook", ]
for (class_name in unique(train_cook_extracted$class)) {
  points_class <- train_cook_extracted[train_cook_extracted$class == class_name, ]
  plot(st_geometry(points_class), add = TRUE, pch = 16, cex = 1, 
       col = class_colors[class_name])
}

# Tumtum
plotRGB(r_tumtum, r = 3, g = 2, b = 1, stretch = "lin", main = "TUMTUM")
train_tumtum_extracted <- all_training_data_sf[all_training_data_sf$site == "tumtum", ]
for (class_name in unique(train_tumtum_extracted$class)) {
  points_class <- train_tumtum_extracted[train_tumtum_extracted$class == class_name, ]
  plot(st_geometry(points_class), add = TRUE, pch = 16, cex = 1, 
       col = class_colors[class_name])
}

# Pyramid
plotRGB(r_pyramid, r = 3, g = 2, b = 1, stretch = "lin", main = "PYRAMID")
train_pyramid_extracted <- all_training_data_sf[all_training_data_sf$site == "pyramid", ]
for (class_name in unique(train_pyramid_extracted$class)) {
  points_class <- train_pyramid_extracted[train_pyramid_extracted$class == class_name, ]
  plot(st_geometry(points_class), add = TRUE, pch = 16, cex = 1, 
       col = class_colors[class_name])
}

dev.off()

## ---- STEP 2: Handle Missing Values ----
cat("\n=== STEP 2: Checking for Missing Values ===\n")

na_summary <- colSums(is.na(all_training_data))
cat("\nNA counts per variable:\n")
print(na_summary[na_summary > 0])

# Remove rows with NAs
rows_before <- nrow(all_training_data)
all_training_data <- all_training_data[complete.cases(all_training_data), ]
rows_after <- nrow(all_training_data)

cat("\nRemoved", rows_before - rows_after, "rows with NAs\n")
cat("Remaining samples:", rows_after, "\n")

## ---- STEP 3: Correlation Analysis ----
cat("\n=== STEP 3: Correlation Analysis ===\n")

# Select only predictor columns
predictor_cols <- !names(all_training_data) %in% c("class", "site")
predictors <- all_training_data[, predictor_cols]

cat("Analyzing", ncol(predictors), "predictors\n")

# Calculate correlation
cor_matrix <- cor(predictors, use = "complete.obs")

# Save correlation matrix
write.csv(cor_matrix, here(output_dir, "correlation_matrix.csv"))
cat("✅ Saved correlation matrix\n")

# Visualize
png(here(output_dir, "correlation_heatmap.png"), 
    width = 3000, height = 3000, res = 300)
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.6,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Predictor Correlation Matrix (All Sites)",
         mar = c(0, 0, 2, 0))
dev.off()
cat("✅ Saved correlation heatmap\n")

# Find high correlations
cor_threshold <- 0.8
high_cor <- which(abs(cor_matrix) > cor_threshold & cor_matrix != 1, arr.ind = TRUE)
high_cor <- high_cor[high_cor[,1] < high_cor[,2], , drop = FALSE]

if (nrow(high_cor) > 0) {
  high_cor_df <- data.frame(
    Variable1 = rownames(cor_matrix)[high_cor[,1]],
    Variable2 = colnames(cor_matrix)[high_cor[,2]],
    Correlation = cor_matrix[high_cor]
  )
  high_cor_df <- high_cor_df[order(-abs(high_cor_df$Correlation)), ]
  
  cat("\nHighly correlated pairs (|r| >", cor_threshold, "):\n")
  print(high_cor_df)
  
  write.csv(high_cor_df, here(output_dir, "high_correlations.csv"), row.names = FALSE)
  cat("✅ Saved high correlations\n")
} else {
  cat("\nNo highly correlated pairs found (threshold =", cor_threshold, ")\n")
}
