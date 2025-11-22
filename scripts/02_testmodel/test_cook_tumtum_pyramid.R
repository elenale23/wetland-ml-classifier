source(here("scripts/01_datacleaning/clean_points.R"))
library(sp)
library(mlr3verse)
library(mlr3spatiotempcv)
library(mlr3spatial)
library(mlr3measures)
library(ggplot2)
library(here)


# In this script, I will re-run a test model on the data points in Cook, Pyramid, and Tum Tum, with added manually digitized points (wetland points for Cook, everything else for Pyramid, wetland and open water for Tum Tum)

## ---- Load additional points from manual digitization ----

added_points_cook_wetland <- read_sf(here("data/points_digitized/cook_wetland_points/cook_wetland_points.shp"))
added_points_pyramid_openwater <- read_sf(here("data/points_digitized/pyramid_points/pyramid_openwater_points.shp"))
added_points_pyramid_wetland <- read_sf(here("data/points_digitized/pyramid_points/pyramid_wetland_points.shp"))
added_points_pyramid_upland <- read_sf(here("data/points_digitized/pyramid_points/pyramid_upland_points.shp"))
added_points_tumtum_wetland <- read_sf(here("data/points_digitized/tumtum_points_extra/tumtum_wetland_points.shp"))
added_points_tumtum_openwater <- read_sf(here("data/points_digitized/tumtum_points_extra/tumtum_openwater_points.shp")) # this one adds to tum tum's existing open water points I manually digitized

added_points_pyramid_openwater <- added_points_pyramid_openwater[,-1] # drop CID column from this data frame
added_points_pyramid_upland <- added_points_pyramid_upland %>% relocate(site) # move site column to the front

# Combine newly added points and examine
added_points <- rbind(added_points_cook_wetland, added_points_pyramid_openwater, added_points_pyramid_wetland, added_points_pyramid_upland, added_points_tumtum_wetland, added_points_tumtum_openwater)
unique(added_points$class)
crs(added_points)

# Combine this with previous training points
all_points <- rbind(raw_and_added, added_points)

summary_2 <- all_points %>% # this is a data frame with original points, added points in three classes for Cook, Pyramid, Tum Tum
  group_by(site,class) %>%
  summarise(count = n(), .groups = "drop")
print(summary_2) # open_water 251, upland 288, wetland 362


## ---- Subset training dataset ----
sites_train <- c("cook", "tum_tum", "pyramid")

training_subset <- all_points %>%
  filter(site %in% sites_train)

training_subset$class <- as.factor(training_subset$class) # ensure "class" is a factor

## ---- Create stack of training points and raster values at each site ----
### --- Load rasters ----
r_cook <- rast(here("data/imagery/cook_jul_25_psscene_analytic_sr_udm2/PSScene/20250704_191552_83_2515_3B_AnalyticMS_SR_harmonized_clip.tif"))
r_tumtum <- rast(here("data/imagery/tumtum_20250715_psscene_analytic_sr_udm2/composite.tif")) # using July 14 raster
r_pyramid <- rast(here("data/imagery/pyramid_jul_4_2025_psscene_analytic_sr_udm2/composite.tif"))
names(r_tumtum) <- c("blue", "green", "red", "nir")

plot(r_pyramid) # sanity check
crs(r_tumtum) # sanity check, again

### ---- Extract values separately for each site, then bind rows ----
# Create a function that takes points and raster, convert points to SpatVector, extract raster values for points, then combine extracted values with original attributes (drop ID from extract)
extract_for_site <- function(points, raster) {
  v <- vect(points)
  vals <- terra::extract(raster, v)
  cbind(st_drop_geometry(points), vals[,-1])
}
# Run the function through the 2 sites
train_cook <- extract_for_site(training_subset %>% filter(site=="cook"), r_cook)
train_tumtum <- extract_for_site(training_subset %>% filter(site=="tum_tum"), r_tumtum)
train_pyramid <- extract_for_site(training_subset %>% filter(site=="pyramid"), r_pyramid)

# Merge training points
training_data <- rbind(train_cook, train_tumtum, train_pyramid)

# Rename coordinates
names(training_data)[names(training_data) == "northing"] <- "y_coord"
names(training_data)[names(training_data) == "easting"] <- "x_coord"

## ---- Create mlr3 classification task ----
rf_task <- as_task_classif_st(
  training_data,
  id = "wetland",
  target = "class",
  coordinate_names = c("x_coord", "y_coord"), 
  coords_as_features = FALSE
)
# Select only spectral bands:
rf_task$select(c("blue", "green", "red", "nir"))

## ---- Create mlr3 Random Forest learner ----
### ---- Create the learner ----
rf_learner <- lrn(
  "classif.ranger", 
  predict_type = "prob", 
  oob.error = TRUE, 
  importance = "impurity")

## --- Setting up hyperparameter search space to later tune different combinations of hyperparatmeters ----
rf_search_space <- ps(
  mtry = p_int(1, 4), # number of parameters to try to fit
  num.trees = p_int(200, 500), # num.trees = number of trees. Need to find the sweet spot between too few and overfitting the data.
  min.node.size = p_int(1, 10),
  max.depth = p_int(4, 20)
)

## ---- Define Spatial Block CV for hyperparameter tuning  ----

# split points into 4 spatial folds
# The field `x_coord` and `y_coord` are numeric UTM coordinates
rf_resampling <- rsmp("repeated_spcv_coords", folds = 3, repeats = 2)
rf_resampling$instantiate(rf_task)
# Visualize fold 1
autoplot(rf_resampling, rf_task, fold_id = 3)

## ---- Evaluate model performance ----
rf_measure <- msr("classif.ce") # measures classification error, compare true observed labels with predicted labels in multiclass classification tasks.

## ---- Create the tuning instance to assemble everything together ----
tuning_instance <- ti(
  task = rf_task,
  resampling = rf_resampling,
  learner = rf_learner,
  measure = rf_measure,
  search_space = rf_search_space,
  terminator = trm("evals", n_evals = 100),
  store_benchmark_result = TRUE,
  store_models = TRUE
)

## ---- Define a tuning strategy an optimize tuner ----

# we use grid_search which is the brute force method that tests every possible combination once
tuner <- tnr("random_search")

# trigger the tuning process with $optimize() and pray
tuner$optimize(tuning_instance)

#see the results of the tuner
tuning_instance$result_learner_param_vals
tuning_instance$result_y

## ---- Train final model all on data with tuned hyperparameters ----
tuned_rf_learner <- lrn("classif.ranger",
                        predict_type = "prob")  # <-- important. This is required for multiclass AUC to work properly
tuned_rf_learner$param_set$values <- tuning_instance$result_learner_param_vals


## ---- Training, validation, and prediction ----
tuned_rf_learner$train(rf_task)

### --- Predicting on each raster ----

# Suppose you have three rasters
rasters <- list(r_cook, r_tumtum, r_pyramid)
site_names <- c("Cook", "TumTum", "Pyramid")  # manually assign names

# Create an empty list to store predictions
predictions <- list()

# Loop through each raster
for (i in seq_along(rasters)) {
  r <- rasters[[i]]
  
  # Predict using the tuned model
  predictions[[site_names[i]]] <- predict_spatial(r, tuned_rf_learner, format = "terra")
  
  # plot the prediction
  plot(predictions[[site_names[i]]], main = paste("Predicted Classes -", site_names[i]))
}

# Loop through and predict + export as GeoTIFF
for (i in seq_along(rasters)) {
  r <- rasters[[i]]
  site <- site_names[i]
  
  # Predict using tuned model
  pred <- predict_spatial(r, tuned_rf_learner, format = "terra")
  
  # Extract raster (if predict_spatial returns a list)
  if (inherits(pred, "list")) pred <- pred[[1]]
  
  # Save raster as GeoTIFF
  output_path <- paste0(here("data/predicted_rasters/predicted_", site, ".tif"))
  writeRaster(pred, filename = output_path, overwrite = TRUE)
  
  message("✅ Saved predicted raster for ", site, " → ", output_path)
}

## confusion matrix
# Predict on the full dataset used in the model
pred_all <- tuned_rf_learner$predict(rf_task)
# Get confusion matrix
cm_all <- pred_all$confusion
cm_all

# compute AUROC for class "wetland" (one-vs-all). Using pROC package because mlr3::measures don't do AUC per class, while multiclass AUC in mlr3 aggregates the measures
library(pROC)

# Extract truth
truth <- pred_all$truth  # factor

# Loop through each class
classes <- levels(truth)
aucs <- sapply(classes, function(cls) {
  # Extract probability of the class
  prob_cls <- pred_all$prob[, cls]
  
  # Create binary truth vector: this class vs all others
  binary_truth <- ifelse(truth == cls, 1, 0)
  
  # Compute AUC using pROC
  roc_obj <- roc(binary_truth, prob_cls)
  auc(roc_obj)
})

aucs

# Calculate multiclass AUC. The AUC can be interpreted as the probability that a randomly chosen positive instance has a higher predicted probability of belonging to the positive class than a randomly chosen negative instance. Therefore, higher values (closer to 1) indicate better performance.
pred_all$score(msr("classif.mauc_aunp")) # AUNP: AUC of each class against the rest, using the a-priori class distribution. Computes the AUC treating a c-dimensional classifier as c two-dimensional 1-vs-rest classifiers, taking into account the prior probability of each class (Fawcett 2001).

## plot per class ROC and compare probability thresholds between accuracy and false positive rate


