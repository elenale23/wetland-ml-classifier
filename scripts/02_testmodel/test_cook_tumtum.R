source("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/scripts/01_datacleaning/clean_points.R")
library(sp)
library(mlr3verse)
library(mlr3spatiotempcv)
library(mlr3spatial)

# In this script, I will run a test model on the data points in Cook and Tum Tum, with added manually digitized points

## ---- Subset training dataset ----
sites_train <- c("cook", "tum_tum")

training_subset <- raw_and_added %>%
  filter(site %in% sites_train)

training_subset$class <- as.factor(training_subset$class) # ensure "class" is a factor

## ---- Create stack of training points and raster values at each site ----

### --- Load rasters ----
r_cook <- rast("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/imagery/cook_jul_25_psscene_analytic_sr_udm2/PSScene/20250704_191552_83_2515_3B_AnalyticMS_SR_harmonized_clip.tif")
r_tumtum <- rast("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/imagery/tumtum_jul_4_2025_psscene_analytic_sr_udm2/composite.tif")

plot(r_tumtum) # sanity check
crs(r_cook) # sanity check, again

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
# Merge training points
training_data <- rbind(train_cook, train_tumtum)

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
  num.trees = p_int(200, 1000), # num.trees = number of trees. Need to find the sweet spot between too few and overfitting the data.
  min.node.size = p_int(1, 10),
  max.depth = p_int(3, 20)
)

## ---- Define Spatial Block CV for hyperparameter tuning  ----

# split points into 4 spatial folds
# The field `x_coord` and `y_coord` are numeric UTM coordinates
rf_resampling <- rsmp("repeated_spcv_coords", folds = 4, repeats = 2)
rf_resampling$instantiate(rf_task)
# Visualize fold 1
autoplot(rf_resampling, rf_task, fold_id = 1)

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
tuner <- tnr("grid_search")

# trigger the tuning process with $optimize() and pray
tuner$optimize(tuning_instance)

#see the results of the tuner
tuning_instance$result_learner_param_vals
tuning_instance$result_y

## ---- Train final model all on data with tuned hyperparameters ----
tuned_rf_learner <- lrn("classif.ranger")
tuned_rf_learner$param_set$values <- tuning_instance$result_learner_param_vals

## ---- Training, validation, and prediction ----
tuned_rf_learner$train(rf_task)

### --- Predicting on each raster ----

# Suppose you have two rasters
rasters <- list(r_cook, r_tumtum)
site_names <- c("Cook", "TumTum")  # manually assign names

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

## cook looks ok, tum tum looks like shit