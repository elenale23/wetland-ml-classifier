# In this script, I have created extra training points for mostly upland class and a small number for other classes. I will run
# the model to see if this improves performance

# Load required libraries
library(mlr3verse)
library(mlr3spatiotempcv)
library(mlr3spatial)
library(mlr3measures)
library(mlr3tuning)
library(mlr3fselect)
library(ggplot2)
library(here)
library(terra)
library(sf)
library(dplyr)
library(tidyterra)
library(corrplot)

## ---- STEP 1: Read in Points Data ----

filtered_points_added <- read_sf("data/points_digitized/points_digitized_02/filtered_points_added.shp")
str(filtered_points_added)
unique(filtered_points_added$site)
st_crs(filtered_points_added)

# Extract geometry column into x and y coords, for later use by as_task_classif_st()

coords <- st_coordinates(filtered_points_added)
filtered_points_added$x_coord <- coords[, 1]
filtered_points_added$y_coord <- coords[, 2]

saveRDS(filtered_points_added, "outputs/cleaned_data/filtered_points_added.RDS")
st_write(filtered_points_added, "outputs/cleaned_data/filtered_points_added.shp")


## ---- STEP 2: Extract Training Data from All Sites ----

# Extract values while preserving original coordinates

output_dir <- here("outputs/extracted_train_data")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

site_dates <- list(
  cook    = "20250704",
  pyramid = "20250704",
  tumtum  = "20250715"
)

all_training_data_added <- NULL

for (site in names(site_dates)) {
  date <- site_dates[[site]]
  cat("Processing site:", site, "| date:", date, "\n")
  
  # Load predictor stack
  predictor_stack_path <- here("outputs/processed_dem", site, date, paste0(site, "_", date, "_stacked_features.tif"))
  predictor_stack <- rast(predictor_stack_path)
  cat("  Loaded", nlyr(predictor_stack), "predictors\n")
  
  # Load and filter training points to current site only
  filtered_points_added <- readRDS(here("outputs/cleaned_data/filtered_points_added.RDS"))
  site_points <- filtered_points_added[filtered_points_added$site == site, ]
  cat("  Points for this site:", nrow(site_points), "\n")
  
  # Extract predictor values
  values <- terra::extract(predictor_stack, site_points)
  
  # Combine site points + extracted values
  site_data <- cbind(site_points, values)
  site_data$ID <- NULL
  
  # Append to master data frame
  all_training_data_added <- rbind(all_training_data_added, site_data)
  
  cat("  Extracted values for", nrow(site_data), "points\n")
}

cat("\n Total training samples:", nrow(all_training_data_added), "\n")
cat("   Sites:", paste(unique(all_training_data_added$site), collapse = ", "), "\n")
cat("   Classes:", paste(unique(all_training_data_added$class), collapse = ", "), "\n")

## ---- STEP 3: Handle Missing Values in Training Data ----

na_summary <- colSums(is.na(all_training_data_added))
cat("\nNA counts per variable:\n")
print(na_summary[na_summary > 0])

# Remove rows with NAs
all_training_data_added <- na.omit(all_training_data_added)
print(colSums(is.na(all_training_data_added)))

# Before saving, ensure class is factor
all_training_data_added$class <- as.factor(all_training_data_added$class)

# Verify
cat("Saving data with class type:", class(all_training_data_added$class), "\n")
cat("Class levels:", paste(levels(all_training_data_added$class), collapse = ", "), "\n")

saveRDS(all_training_data_added, here("outputs/extracted_train_data/all_training_data_added.rds"))

# Save training data as shp file if needed
last_extent <- st_read(here("data/raster_extents", paste0(sites[length(sites)], "_raster_extent.shp")), quiet = TRUE)
extent_crs <- st_crs(last_extent)
all_training_data_added_sf <- st_as_sf(all_training_data_added, 
                                       coords = final_coord_cols,
                                       crs = extent_crs)
st_write(all_training_data_added_sf, "outputs/extracted_train_data/all_training_data_added.shp", delete_dsn = TRUE, quiet = TRUE)

## ---- STEP 4: Plot points on RGB for sanity check ----

class_colors <- c("wetland" = "orange", "upland" = "red", "open_water" = "blue")

# Set up 3-panel plot
par(mfrow = c(1, 3), mar = c(2, 2, 3, 1))

# Cook
r_cook <- rast("data/cropped_wetland_rasters/cook/cook_20250704.tif")
plotRGB(r_cook, r = 3, g = 2, b = 1, stretch = "lin", main = "COOK")
train_cook_extracted <- all_training_data_added[all_training_data_added$site == "cook", ]
for (class_name in unique(train_cook_extracted$class)) {
  points_class <- train_cook_extracted[train_cook_extracted$class == class_name, ]
  plot(st_geometry(points_class), add = TRUE, pch = 16, cex = 1, 
       col = class_colors[class_name])
}

# Tumtum
r_tumtum <- rast("data/cropped_wetland_rasters/tumtum/tumtum_20250715.tif")
plotRGB(r_tumtum, r = 3, g = 2, b = 1, stretch = "lin", main = "TUMTUM")
train_tumtum_extracted <- all_training_data_added[all_training_data_added$site == "tumtum", ]
for (class_name in unique(train_tumtum_extracted$class)) {
  points_class <- train_tumtum_extracted[train_tumtum_extracted$class == class_name, ]
  plot(st_geometry(points_class), add = TRUE, pch = 16, cex = 1, 
       col = class_colors[class_name])
}

# Pyramid
r_pyramid <- rast ("data/cropped_wetland_rasters/pyramid/pyramid_20250704.tif")
plotRGB(r_pyramid, r = 3, g = 2, b = 1, stretch = "lin", main = "PYRAMID")
train_pyramid_extracted <- all_training_data_added[all_training_data_added$site == "pyramid", ]
for (class_name in unique(train_pyramid_extracted$class)) {
  points_class <- train_pyramid_extracted[train_pyramid_extracted$class == class_name, ]
  plot(st_geometry(points_class), add = TRUE, pch = 16, cex = 1, 
       col = class_colors[class_name])
}

par(mfrow = c(1, 1))
dev.off()

## ---- STEP 4.5: Create Train/Test Split ----
# The "test set," is held out from the very beginning of the project and is not used in any part of the training or cross-validation process.
# Use this test set to measure final, trained model OOB error and accuracy.

# Stratified split by class to ensure all classes represented in both sets

test_idx <- c()
for (cls in unique(all_training_data_added$class)) {
  cls_idx <- which(all_training_data_added$class == cls)
  n_test <- round(length(cls_idx) * 0.2)  # 80/20 split
  test_idx <- c(test_idx, sample(cls_idx, n_test))
}

train_data <- all_training_data_added[-test_idx, ]
test_data  <- all_training_data_added[test_idx, ]

cat("Training samples:", nrow(train_data), "\n")
cat("Test samples:", nrow(test_data), "\n")

# Save test set and lock it away — don't touch until final evaluation
saveRDS(test_data, here("outputs/extracted_train_data/test_data.rds"))
saveRDS(train_data, here("outputs/extracted_train_data/train_data.rds"))

## ---- STEP 5: Create mlr3 task ----

rf_task_added <- as_task_classif_st(
  train_data,          # train only, not all_training_data_added
  id = "wetland",
  target = "class",
  coordinate_names = c("x_coord", "y_coord"),
  coords_as_features = FALSE
)

rf_task_added$col_roles$feature <- setdiff(
  rf_task_added$col_roles$feature,
  c("site", "x_coord", "y_coord")
)
# Verify they're excluded
cat("Task features:", paste(rf_task_added$feature_names, collapse = ", "), "\n")

# Verify task preserved factor
cat("\nTask created successfully\n")
cat("Task target type:", class(rf_task_added$data()$class), "\n")
cat("Task target is factor:", is.factor(rf_task_added$data()$class), "\n")
cat("Task target levels:", paste(levels(rf_task_added$data()$class), collapse = ", "), "\n")

## ---- STEP 6: Create mlr3 Random Forest learner ----
rf_learner_added <- lrn(
  "classif.ranger", 
  predict_type = "prob", 
  oob.error = TRUE, 
  importance = "impurity"
)

## ---- STEP 7: Simple forward feature selection to choose model variables ----

seeds <- c(42, 123, 456) # Run feature selection with 3 random seeds, then choose the most consistent variables
selection_results <- list()

for (s in seeds) {
  set.seed(s)
  cat("Running feature selection with seed:", s, "\n")
  
  instance <- fselect(
    fselector = fs("sequential"),
    task = rf_task_added,
    learner = rf_learner_added,
    resampling = rsmp("repeated_spcv_coords", folds = 3, repeats = 1),
    measure = msr("classif.acc")
  )
  
  selection_results[[as.character(s)]] <- instance$result_feature_set
  cat("  Selected:", paste(instance$result_feature_set, collapse = ", "), "\n")
}

# Model performance in iterations of sequential forward selection 
autoplot(instance, type = "performance")
dt <- as.data.table(instance$archive)
dt[batch_nr == 7, 1:12] # examine batch 6

# See which features appear across all runs
all_selected <- unlist(selection_results)
feature_counts <- sort(table(all_selected), decreasing = TRUE)
print(feature_counts)

# See which features are selected 
instance$result_feature_set

# Select features for task
rf_task_added$select(instance$result_feature_set)

cat("Task features:", paste(rf_task_added$feature_names, collapse = ", "), "\n")
cat("Number of observations:", rf_task_added$nrow, "\n")

## ---- STEP 8: Hyperparameter tuning, Spatial Cross-Validation, Run and optimize tuner ---- 

# Setting up hyperparameter search space
rf_search_space_added <- ps(
  mtry = p_int(2, 6),  
  num.trees = p_int(300, 1000),
  min.node.size = p_int(5, 20),
  max.depth = p_int(10, 30),
  sample.fraction = p_dbl(lower = 0.5, upper = 1)
)

# Define Spatial Block CV
rf_resampling_added <- rsmp("repeated_spcv_coords", folds = 3, repeats = 2)

# Instantiating resampling on task
rf_resampling_added$instantiate(rf_task_added)
cat("Number of iterations:", rf_resampling_added$iters, "\n")

# Visualize spatial folds (wrap in try() in case it fails)
tryCatch({
  p <- autoplot(rf_resampling_added, rf_task_added, fold_id = 1)
  print(p)
})

# Evaluate model performance
rf_measure_added <- msr("oob_error")

cat("\nPerformance measure:", rf_measure_added$id, "\n")

# Create the tuning instance
tuning_instance_added <- ti(
  task = rf_task_added,
  resampling = rf_resampling_added,
  learner = rf_learner_added,
  measure = rf_measure_added,
  search_space = rf_search_space_added,
  terminator = trm("evals", n_evals = 50),
  store_benchmark_result = TRUE,
  store_models = TRUE
)

# Define tuner and optimize 
tuner_added <- tnr("grid_search", resolution = 5)
cat("Tuner type:", class(tuner_added)[1], "\n")

# Start hyperparameter tuning, this might take a while...
tuner_added$optimize(tuning_instance_added)

# See best hyperparameters
print(tuning_instance_added$result_learner_param_vals)
cat("\n OOB Error:", tuning_instance_added$result_y, "\n")

# Save tuning results
saveRDS(tuning_instance_added, "outputs/models/tuning_instance.rds")
best_params_added <- tuning_instance$result_learner_param_vals
saveRDS(best_params_added, "outputs/models/best_hyperparameters_added.rds")

## ---- STEP 9: Train final model with tuned hyperparameters ----
tuned_rf_learner_added <- lrn("classif.ranger", 
                                  predict_type = "prob",
                                  oob.error = TRUE, 
                                  importance = "impurity",
                                  mtry = 4,
                                  num.trees = 1000,
                                  min.node.size = 5,
                                  max.depth = 10,
                                  sample.fraction = 0.875
)

# Train model on train_data (80% of the training dataset)
tuned_rf_learner_added$train(rf_task_added)

# Save the trained learner
saveRDS(tuned_rf_learner_added, "outputs/models/tuned_rf_learner_added.rds")

## ---- STEP 10: Examine feature importance ----

importance_scores <- tuned_rf_learner_added$model$variable.importance
importance_df <- data.frame(
  Feature = names(importance_scores),
  Importance = importance_scores
) %>%
  arrange(desc(Importance))

print(importance_df)

# Plot: All features ranked
p1 <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +  
  coord_flip() +
  labs(x = NULL,
       y = "Variable Importance",
       title = NULL) +
  theme_classic() +
  theme(legend.position = "bottom")

print(p1)
dev.off()

ggsave(here("outputs/feature_importance_added.png"), p1, width = 10, height = 7, dpi = 300)

## ---- STEP 10.5: Correlation Analysis ----

# Select only predictor columns
predictor_cols <- !names(all_training_data_added) %in% c("class", "site", "x_coord", "y_coord")
predictors <- all_training_data_added[, predictor_cols]
predictors$geometry <- NULL

cat("Analyzing", ncol(predictors), "predictors\n")

# Calculate correlation
cor_matrix <- cor(predictors, use = "complete.obs")

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
} else {
  cat("\nNo highly correlated pairs found (threshold =", cor_threshold, ")\n")
}

## ---- STEP 11: Final Model Evaluation on Held-Out Test Set ----

# Load test set and prepare identically to train data
test_data <- readRDS(here("outputs/extracted_train_data/test_data.rds"))
test_data$class <- as.factor(test_data$class)

# Predict on test set
test_sf <- st_as_sf(test_data)  # ensure it's sf if needed
test_pred <- tuned_rf_learner_added$predict_newdata(test_data)

# mlr3 built-in measures on test predictions

test_pred$score(msrs(c(
  "classif.acc", # Classification accuracy
  "classif.mcc", # Matthews correlation coefficient, uses all four values of the confusion matrix. 
  "classif.ce", # Classification error
  "classif.mauc_mu" # Multiclass AUC as defined in Kleinman and Page (2019). This measure is an average of the pairwise AUCs between all classes.
)))

# For producer/user accuracy, kappa, F1 — use the confusion matrix directly
cm <- as.matrix(confusion)
print(cm)

# Producer accuracy (recall per class = TP / column sum), probability that a particular sample of class c is mapped as the same class c in the classification map aka how often are real features on the ground correctly shown on the classified map?
producer_acc <- diag(cm) / colSums(cm)
cat("\nProducer Accuracy:\n"); print(producer_acc)

# User accuracy (precision per class = TP / row sum), probability that a particular map location of class c is also the same class c in truth aka how often the class on the map will actually be present on the ground?
user_acc <- diag(cm) / rowSums(cm)
cat("\nUser Accuracy:\n"); print(user_acc)

# F1 per class (the harmonic mean (or a weighted average) of precision and recall)
f1 <- 2 * (producer_acc * user_acc) / (producer_acc + user_acc)
cat("\nF1 per class:\n"); print(f1)

# Overall kappa, measures agreement between predictions and actual labels; compares the overall accuracy to the expected random chance accuracy
acc <- sum(diag(cm)) / sum(cm) # The observed proportion of agreement.
expected_acc <- sum(rowSums(cm) * colSums(cm)) / sum(cm)^2 # The expected proportion of agreement by chance
kappa <- (acc - expected_acc) / (1 - expected_acc)
cat("\nKappa:", kappa, "\n")

## ---- STEP 12: Train model on ALL of training data ----

# Create task for full training data
rf_task_final <- as_task_classif_st(
  all_training_data_added,   # full dataset 
  id = "wetland",
  target = "class",
  coordinate_names = c("x_coord", "y_coord"),
  coords_as_features = FALSE
)

# Ensure that "site" and coordinate columns are not recognize as features
rf_task_final$col_roles$feature <- setdiff( 
  rf_task_final$col_roles$feature,
  c("site", "x_coord", "y_coord")
)

# Select the same features as the first model
rf_task_final$select(instance$result_feature_set)
cat("Task features:", paste(rf_task_final$feature_names, collapse = ", "), "\n")
cat("Number of observations:", rf_task_final$nrow, "\n")

# Train the final model
tuned_rf_learner_added$train(rf_task_final)

## ---- STEP 13: Predict on each raster site ----

# Load the rasters with 12 stacked features
r_cook <- rast("outputs/processed_dem/cook/20250704/cook_20250704_stacked_features.tif")
r_tumtum <- rast("outputs/processed_dem/tumtum/20250715/tumtum_20250715_stacked_features.tif")
r_pyramid <- rast("outputs/processed_dem/pyramid/20250704/pyramid_20250704_stacked_features.tif")

plot(r_cook)
plot(r_pyramid)
plot(r_tumtum)

# Get expected features from model 
feature_names <- tuned_rf_learner_added$model$forest$independent.variable.names

cat("\nModel expects these features (n=", length(feature_names), "):\n")

cat(paste(feature_names, collapse = ", "), "\n")

cat("\nOriginal raster has these bands (n=", nlyr(r_cook), "):\n")
cat(paste(names(r_cook), collapse = ", "), "\n")

# Check if all expected features are present
missing_features <- setdiff(feature_names, names(r_cook))
if (length(missing_features) > 0) {
  stop("ERROR: Raster is missing features: ", paste(missing_features, collapse = ", "))
}

# SELECT only the features the model needs
r_cook <- r_cook[[feature_names]]
r_tumtum <- r_tumtum[[feature_names]]
r_pyramid <- r_pyramid[[feature_names]]

plot(r_cook)
plot(r_pyramid)
plot(r_tumtum)

# Predict on each raster site
pred_cook_20250704 <- predict(r_cook, tuned_rf_learner_added)
plot(pred_cook_20250704)

pred_pyramid <- predict(r_pyramid, tuned_rf_learner_added)
plot(pred_pyramid)

pred_tumtum <- predict(r_tumtum, tuned_rf_learner_added)
plot(pred_tumtum)

# what if I predict on a date with no training data?
r_cook_20250704 <- rast("data/dem_processed/cook/cook_20250704_stacked_features.tif")
pred_cook_20250704 <- predict(r_cook_20250704, tuned_rf_learner_added)
plot(pred_cook_20250704)

# crop predicted rasters to exact wetland extent
# Crop predictions to extent
kml_path <- ("data/wetland_polygons/cook_polygon.kml")
polygon <- st_read(kml_path)
polygon <- st_transform(polygon, st_crs(pred_cook_20250704))
polygon_vect <- vect(polygon)

r_cropped <- crop(pred_cook_20250704, polygon_vect)
r_masked <- mask(r_cropped, polygon_vect)

plot(r_masked, main = "cook 2025-09-24")

## ---- Plot with RGB and overlay and no upland 
rgb <- rast("data/cropped_wetland_rasters/tumtum_20250715.tif")

# Mask upland
r_masked[r_masked == 2] <- NA

pred_tumtum[pred_tumtum == 2] <- NA

# Plot
par(mfrow = c(1, 2), mar = c(1, 1, 2, 1))

plotRGB(rgb, r = 3, g = 2, b = 1, stretch = "lin", 
        main = "RGB", axes = FALSE)

plotRGB(rgb, r = 3, g = 2, b = 1, stretch = "lin", 
        main = "Wetland Prediction", axes = FALSE)
plot(pred_tumtum, col = c("blue", NA, "orange"), add = TRUE, alpha = 0.6, legend = FALSE, main = "Cook = 2025-09-24")

par(mfrow = c(1, 1))
dev.off()

## ---- STEP 14: Area calculation ----
levels(r_masked)
st_crs(r_masked)

# Calculate area
pixel_area <- prod(res(r_masked))  # width × height of one pixel in m²
open_water_pixels <- sum(values(r_masked) == 1, na.rm = TRUE)
open_water_area_m2 <- open_water_pixels * pixel_area

cat("Open water area:", open_water_area_m2, "m²\n")