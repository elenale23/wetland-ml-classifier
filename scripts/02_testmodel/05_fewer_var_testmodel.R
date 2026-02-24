## THis script will test the model with fewer variables

# Load required libraries
library(sp)
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
library(pROC)
library(tidyterra)

# Load the cleaned, filtered, and stacked training data from RDS file
all_training_data <- readRDS(here("outputs/feature_selection/all_training_data.rds"))
cat("✅ Loaded all training data from RDS file\n")

# Note: Run data cleaning, DEM processing, point filtering, and feature selection once before running this script

# Rename coordinates (only do this once per run)
names(all_training_data)[names(all_training_data) == "northing"] <- "y_coord"
names(all_training_data)[names(all_training_data) == "easting"] <- "x_coord"

as.numeric(all_training_data$x_coord)
as.numeric(all_training_data$y_coord)

# Remove some variables
fewer_var_training_data <- select(all_training_data, -c(twi, ndvi))

cat("\n--- Training Data Summary ---\n")
cat("Total observations:", nrow(fewer_var_training_data), "\n")
cat("Class distribution:\n")
print(table(fewer_var_training_data$class))
cat("Class type:", class(fewer_var_training_data$class), "\n")

## ---- Check for NA values (important with DEM data) ----
cat("\nChecking for NA values in training data:\n")
na_summary <- colSums(is.na(fewer_var_training_data))
print(na_summary)

# Remove rows with NA values if any exist
if (sum(na_summary) > 0) {
  cat("\nWarning: Removing", sum(complete.cases(training_data) == FALSE), 
      "rows with NA values\n")
  training_data <- training_data[complete.cases(training_data), ]
}

## ---- Create mlr3 classification task ----
cat("\n--- Creating mlr3 Task ---\n")

# Create task
fewer_var_rf_task <- as_task_classif_st(
  fewer_var_training_data,
  id = "wetland",
  target = "class",
  coordinate_names = c("x_coord", "y_coord"), 
  coords_as_features = FALSE)

# Verify task preserved factor
cat("\nTask created successfully\n")
cat("Task target type:", class(fewer_var_rf_task$data()$class), "\n")
cat("Task target is factor:", is.factor(fewer_var_rf_task$data()$class), "\n")
cat("Task target levels:", paste(levels(fewer_var_rf_task$data()$class), collapse = ", "), "\n")

## ---- Simple forward feature selection

instance = fselect(
  fselector = fs("sequential"),
  task =  fewer_var_rf_task,
  learner = fewer_var_rf_learner,
  resampling = rsmp("repeated_spcv_coords", folds = 5),
  measure = msr("classif.acc")
)

# See which features are selected (this might take a while. might not need to repeat if already know which variables to use)
instance$result_feature_set

# Select features for task
fewer_var_rf_task$select(instance$result_feature_set)

cat("Task features:", paste(fewer_var_rf_task$feature_names, collapse = ", "), "\n")
cat("Number of observations:", fewer_var_rf_task$nrow, "\n")

## ---- Create mlr3 Random Forest learner ----
fewer_var_rf_learner <- lrn(
  "classif.ranger", 
  predict_type = "prob", 
  oob.error = TRUE, 
  importance = "impurity"
)

## ---- Setting up hyperparameter search space ----
fewer_var_rf_search_space <- ps(
  mtry = p_int(2, 6),  
  num.trees = p_int(300, 1000),
  min.node.size = p_int(5, 20),
  max.depth = p_int(10, 30),
  sample.fraction = p_dbl(lower = 0.5, upper = 1)
)

## ---- Define Spatial Block CV ----
cat("\n--- Setting up Spatial Cross-Validation ---\n")
fewer_var_rf_resampling <- rsmp("repeated_spcv_coords", folds = 10, repeats = 1)

cat("Instantiating resampling on task...\n")
fewer_var_rf_resampling$instantiate(fewer_var_rf_task)

cat("✅ Resampling instantiated successfully\n")
cat("Number of iterations:", fewer_var_rf_resampling$iters, "\n")

# Visualize spatial folds (wrap in try() in case it fails)
cat("\nCreating fold visualization...\n")
tryCatch({
  p <- autoplot(fewer_var_rf_resampling, fewer_var_rf_task, fold_id = 1)
  print(p)
  cat("✅ Fold visualization created\n") 
  }, error = function(e) {
  cat("⚠️  Could not create visualization:", e$message, "\n")
})

## ---- Evaluate model performance ----
fewer_var_rf_measure <- msr("oob_error")
  
cat("\nPerformance measure:", fewer_var_rf_measure$id, "\n")

## ---- Create the tuning instance ----
cat("\n--- Creating Tuning Instance ---\n")

fewer_var_tuning_instance <- ti(
  task = fewer_var_rf_task,
  resampling = fewer_var_rf_resampling,
  learner = fewer_var_rf_learner,
  measure = fewer_var_rf_measure,
  search_space = fewer_var_rf_search_space,
  terminator = trm("evals", n_evals = 100),
  store_benchmark_result = TRUE,
  store_models = TRUE
)

cat("✅ Tuning instance created successfully\n")

## ---- Define tuner and optimize ----
cat("\n--- Defining Tuner ---\n")
fewer_var_tuner <- tnr("grid_search", resolution = 5)
cat("Tuner type:", class(fewer_var_tuner)[1], "\n")

cat("\nStarting hyperparameter tuning...\n")
cat("This may take a while...\n\n")

fewer_var_tuner$optimize(fewer_var_tuning_instance)

cat("\n✅ Tuning complete!\n")
cat("Best hyperparameters:\n")
print(fewer_var_tuning_instance$result_learner_param_vals)
cat("\n OOB Error:", fewer_var_tuning_instance$result_y, "\n")


## ---- Train final model with tuned hyperparameters ----
fewer_var_tuned_rf_learner <- lrn("classif.ranger", 
                                  predict_type = "prob",
                                  oob.error = TRUE, 
                                  importance = "impurity",
                                  mtry = 6,
                                  num.trees = 100,
                                  min.node.size = 5,
                                  max.depth = 25
                                  )

cat("\nTraining final model...\n")
fewer_var_tuned_rf_learner$train(fewer_var_rf_task)

## ---- Save Tuning Results ----

# After tuning completes
saveRDS(tuning_instance, "outputs/models/tuning_instance.rds")
best_params <- tuning_instance$result_learner_param_vals
saveRDS(best_params, "outputs/models/best_hyperparameters.rds")

# Also save the trained learner
saveRDS(tuned_rf_learner, "outputs/models/tuned_rf_learner.rds")

## ---- Examine feature importance ----
cat("\n========================================\n")
cat("Feature Importance Analysis\n")
cat("========================================\n")

importance_scores <- fewer_var_tuned_rf_learner$model$variable.importance
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

ggsave(here("outputs/feature_importance_fewervar.png"), p1, width = 10, height = 7, dpi = 300)
cat("\n✅ Saved: outputs/feature_importance_fewervar.png\n")

## ---- Save feature importance to CSV ----
write.csv(importance_df, 
          here("outputs/feature_importance_detailed.csv"), 
          row.names = FALSE)

cat("\n✅ Feature importance tables saved\n")

## ---- Predict on each site  ----

## ---- Crop rasters to wetland polygon ---- 

# Load the STACKED rasters (with all 12 features)
r_cook <- rast("data/dem_processed/cook_stacked_features.tif")
r_tumtum <- rast("data/dem_processed/tumtum_stacked_features.tif")
r_pyramid <- rast("data/dem_processed/pyramid_stacked_features.tif")

plot(r_cook)
plot(r_pyramid)
plot(r_tumtum)

cat("\n========================================\n")
cat("Preparing Rasters for Prediction\n")
cat("========================================\n")

# Get expected features from model 
expected_names <- fewer_var_tuned_rf_learner$model$forest$independent.variable.names

cat("\nModel expects these features (n=", length(expected_names), "):\n")
cat(paste(expected_names, collapse = ", "), "\n")

cat("\nOriginal raster has these bands (n=", nlyr(r_cook), "):\n")
cat(paste(names(r_cook), collapse = ", "), "\n")

# Check if all expected features are present
missing_features <- setdiff(expected_names, names(r_cook))
if (length(missing_features) > 0) {
  stop("ERROR: Raster is missing features: ", paste(missing_features, collapse = ", "))
}

# SELECT only the features the model needs
cat("\nSelecting required features from rasters...\n")
r_cook <- r_cook[[expected_names]]
r_tumtum <- r_tumtum[[expected_names]]
r_pyramid <- r_pyramid[[expected_names]]

plot(r_cook)
plot(r_pyramid)
plot(r_tumtum)

# Verify selection
cat("✅ Cook raster bands:", names(r_cook), "\n")
cat("✅ Number of bands:", nlyr(r_cook), "\n")
cat("✅ Order matches expected:", all(names(r_cook) == expected_names), "\n")

pred_cook_20250704 <- predict(r_cook, fewer_var_tuned_rf_learner)
plot(pred_cook_20250704)

pred_pyramid <- predict(r_pyramid, fewer_var_tuned_rf_learner)
plot(pred_pyramid)

pred_tumtum <- predict(r_tumtum, fewer_var_tuned_rf_learner)
plot(pred_tumtum)


# what if I predict on a date with no training data?
r_cook_20250704 <- rast("data/dem_processed/cook/cook_20250704_stacked_features.tif")
pred_cook_20250704 <- predict(r_cook_20250704, fewer_var_tuned_rf_learner)
plot(pred_cook_20250704)


# crop predicted rasters to exact wetland extent
# Crop predictions to extent
kml_path <- ("data/wetland_polygons/cook_polygon.kml")
polygon <- st_read(kml_path)
polygon <- st_transform(polygon, st_crs(pred_cook_20250609))
polygon_vect <- vect(polygon)

r_cropped <- crop(pred_cook_20250704, polygon_vect)
r_masked <- mask(r_cropped, polygon_vect)

plot(r_masked, main = "cook 2025-09-24")

## ---- Plot with RGB and overlay and no upland ---- ##

## ---- Minimal Clean Plot ----

rgb <- rast("data/cropped_wetland_rasters/cook_20250704.tif")

# Mask upland
r_masked[r_masked == 2] <- NA

# Plot
par(mfrow = c(1, 2), mar = c(1, 1, 2, 1))

plotRGB(rgb, r = 3, g = 2, b = 1, stretch = "lin", 
        main = "RGB", axes = FALSE)

plotRGB(rgb, r = 3, g = 2, b = 1, stretch = "lin", 
        main = "Wetland Prediction", axes = FALSE)
plot(r_masked, col = c("blue", NA, "orange"), add = TRUE, alpha = 0.6, legend = FALSE, main = "Cook = 2025-09-24")

par(mfrow = c(1, 1))

## ---- Model Performance Metrics ----
cat("\n========================================\n")
cat("Model Performance\n")
cat("========================================\n")

# Predict on full training dataset
pred_all <- fewer_var_tuned_rf_learner$predict(fewer_var_rf_task)

# Confusion matrix
cm_all <- pred_all$confusion
cat("\nConfusion Matrix:\n")
print(cm_all)

# Per-class metrics
cat("\nPer-class accuracy:\n")
diag_vals <- diag(cm_all)
class_totals <- rowSums(cm_all)
class_acc <- diag_vals / class_totals
print(round(class_acc, 3))

# Overall accuracy
overall_acc <- sum(diag_vals) / sum(cm_all)
cat("\nOverall Accuracy:", round(overall_acc, 3), "\n")

# Per-class AUC (one-vs-rest)

truth <- pred_all$truth
classes <- levels(truth)

aucs <- sapply(classes, function(cls) {
  prob_cls <- pred_all$prob[, cls]
  binary_truth <- ifelse(truth == cls, 1, 0)
  roc_obj <- roc(binary_truth, prob_cls, quiet = TRUE)
  auc(roc_obj)
})

cat("\nPer-class AUC (one-vs-rest):\n")
print(round(aucs, 3))

# Multiclass AUC
mauc <- pred_all$score(msr("classif.mauc_aunp"))
cat("\nMulticlass AUC (AUNP):", round(mauc, 3), "\n")

## ---- Save performance metrics ----
performance_summary <- data.frame(
  Metric = c("Overall_Accuracy", "Multiclass_AUC", 
             paste0("AUC_", names(aucs)),
             paste0("Accuracy_", names(class_acc))),
  Value = c(overall_acc, mauc, aucs, class_acc)
)

print(performance_summary)

write.csv(performance_summary, 
          here("outputs/model_performance_with_topo.csv"), 
          row.names = FALSE)

cat("\n✅ All analysis complete!\n")
cat("Performance metrics saved to: outputs/model_performance_with_topo.csv\n")

## ---- Pretty Confusion Matrix with ggplot2 ----

cm_all <- pred_all$confusion

# Convert to matrix
cm_all <- as.matrix(cm_all)

cat("Confusion Matrix:\n")
print(cm_all)

# Calculate totals
total_per_class <- rowSums(cm_all)
cat("\nTotal per class:\n")
print(total_per_class)

# Convert to data frame for plotting
conf_df <- as.data.frame(cm_all)

# The structure should be:
# Truth   Prediction   Freq
# But we need to add Truth as a separate column

# Actually, when you do as.data.frame on a table, it gives you:
# Var1 Var2 Freq format
# So let's use as.data.frame.matrix instead

conf_df <- as.data.frame.matrix(cm_all)
conf_df$Truth <- rownames(conf_df)

cat("\nconf_df:\n")
print(conf_df)

# Melt to long format
conf_long <- melt(conf_df, 
                  id.vars = "Truth", 
                  variable.name = "Prediction", 
                  value.name = "Count")

cat("\nconf_long:\n")
print(head(conf_long, 9))

# Calculate percentages
conf_long$Truth <- as.character(conf_long$Truth)
conf_long$Percentage <- (conf_long$Count / total_per_class[conf_long$Truth]) * 100

# Plot
p_conf <- ggplot(conf_long, aes(x = Prediction, y = Truth, fill = Count)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", Count, Percentage)), 
            size = 5, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Confusion Matrix",
       x = "Predicted Class",
       y = "True Class") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  coord_fixed()

print(p_conf)

ggsave("outputs/figures/confusion_matrix.png", 
       plot = p_conf, 
       width = 8, 
       height = 7, 
       dpi = 300)


