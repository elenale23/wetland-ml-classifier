library(sf)
library(terra)
library(dplyr)
library(writexl)

## ---- Cleaning raw data points ----
# Read in wetland points as shapefiles 
points_cook <- read_sf("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/points_raw/Cook.shp")
points_goose <- read_sf("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/points_raw/Goose.shp")
points_lostcreek <- read_sf("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/points_raw/Lost Creek.shp")
points_pyramid <- read_sf("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/points_raw/Pyramid.shp")
points_tumtum <- read_sf("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/points_raw/Tum Tum.shp")

# Create column with site names
points_cook <- points_cook %>% mutate(site = "Cook") %>% select(-Name)
points_goose <- points_goose %>% mutate(site = "Goose")
points_lostcreek <- points_lostcreek %>% mutate(site = "LostCreek")
points_pyramid <- points_pyramid %>% mutate(site = "Pyramid")
points_tumtum <- points_tumtum %>% mutate(site = "TumTum")

# Some sites have a "Desc1" and "Desc2" column but others don't (e.g., Cook), so first, I will train a model without the wetland subclasses
# I will remove the "Desc1", "Desc2" columns,  and rows with the "base" value from all dataframes
points_list_exc_cook <- list(points_goose = points_goose, points_lostcreek = points_lostcreek, points_pyramid = points_pyramid, points_tumtum = points_tumtum)
columns_to_remove <- c("Desc1", "Desc2", "Name", "Elevation")
values_to_remove <- "base"

# Use lapply to apply column and row removal to each dataframe
points_list_exc_cook_cleaned <- lapply(points_list_exc_cook, function(points_list_exc_cook) {
  points_list_exc_cook %>% 
    select(-columns_to_remove) %>%
    filter(Code != values_to_remove)
})

# overwrite original point dataframes
list2env(points_list_exc_cook_cleaned, envir = .GlobalEnv)

# Combine shapefiles
points_all_cleaned <- rbind(points_cook, points_goose, points_lostcreek, points_pyramid, points_tumtum)

# switch around column orders and standardize naming
points_all_cleaned <- points_all_cleaned %>%
  rename(Site = site) %>%
  select(Site, Code, everything())

# Examining the structure of the dataframe
unique(points_all_cleaned$Code)

# There's 3 points called "Wetland-Polygon", so I will remove them
points_all_cleaned <- points_all_cleaned %>%
  filter(Code != "Wetland-Polygon")

# Drop ZM dimension 
points_all_cleaned <- st_zm(points_all_cleaned, what = "ZM")

# I want to know the total number of points by site and class
summary <- points_all_cleaned %>%
  group_by(Site, Code) %>%
  summarise(count = n(), .groups = "drop")
print(summary)

# Clearly, there is a class imbalance issue, with more wetland points than open water and upland points. Number of points between sites are also unequal. 
# Because of this, I will manually digitize more "ground-truth" points to train open water and upland areas. 

# But first, set crs to NAD83 / UTM Zone 11
points_all_cleaned <- st_set_crs(points_all_cleaned, 32611)
crs(points_all_cleaned) 

# Write shp file
st_write(points_all_cleaned, "/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/outputs/01_cleaneddata/points_all_cleaned.shp", delete_dsn = TRUE)

## ---- Load additional points from manual digitization ----

added_points_cook_water <- read_sf("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/points_digitized/openwater_cook.shp")
added_points_cook_upland <- read_sf("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/points_digitized/upland_cook.shp")
added_points_tumtum_water <- read_sf("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/points_digitized/tumtum_openwater_points5.shp")
added_points_tumtum_upland <- read_sf("/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/data/points_digitized/tumtum_upland_points2.shp")

# Combine files
added_points_combined <- rbind(added_points_cook_upland, added_points_cook_water, added_points_tumtum_upland, added_points_tumtum_water)

# Reproject original points to WGS84/UTM11
transformed_allpoints <- st_transform(points_all_cleaned, crs = 32611)
crs(transformed_allpoints)

# Combine raw and added data points
raw_and_added <- rbind(points_all_cleaned, added_points_combined)
raw_and_added <- raw_and_added %>% arrange(Site) # Sort by site

# Rename columns and classes
names(raw_and_added)[names(raw_and_added) == "Site"] <- "site"
names(raw_and_added)[names(raw_and_added) == "Code"] <- "class"
names(raw_and_added)[names(raw_and_added) == "Northing"] <- "northing"
names(raw_and_added)[names(raw_and_added) == "Easting"] <- "easting"

raw_and_added <- raw_and_added %>%
  mutate(class = case_when(
    class %in% c("Wetland-Point") ~ "wetland",
    class %in% c("Open Water-Point") ~ "open_water",
    class %in% c("Upland-Point") ~ "upland"
  ))

raw_and_added <- raw_and_added %>%
  mutate(site = case_when(
         site %in% c("Cook") ~ "cook",
         site %in% c("Goose") ~ "goose",
         site %in% c("LostCreek") ~ "lost_creek",
         site %in% c("Pyramid") ~ "pyramid",
         site %in% c("TumTum") ~ "tumtum"
         ))

summary_1 <- raw_and_added %>% ## this is a data frame with original field-collected points, and round 1 of manually digitized points for Cook and Tumtum's open water and upland classes
  group_by(site, class) %>%
  summarise(count = n(), .groups = "drop")
print(summary_1) # open_water 152, upland 194, wetland 257


## ---- I decided to add more manually digitized points (round 2). Load additional points here ----
added_points_cook_wetland <- read_sf(here("data/points_digitized/cook_wetland_points/cook_wetland_points.shp"))
added_points_pyramid_openwater <- read_sf(here("data/points_digitized/pyramid_points/pyramid_openwater_points.shp"))
added_points_pyramid_wetland <- read_sf(here("data/points_digitized/pyramid_points/pyramid_wetland_points.shp"))
added_points_pyramid_upland <- read_sf(here("data/points_digitized/pyramid_points/pyramid_upland_points.shp"))
added_points_tumtum_wetland <- read_sf(here("data/points_digitized/tumtum_points_extra/tumtum_wetland_points.shp"))
added_points_tumtum_openwater <- read_sf(here("data/points_digitized/tumtum_points_extra/tumtum_openwater_points.shp"))

added_points_pyramid_openwater <- added_points_pyramid_openwater[,-1]
added_points_pyramid_upland <- added_points_pyramid_upland %>% relocate(site)

# Combine newly added points
added_points <- rbind(added_points_cook_wetland, added_points_pyramid_openwater, 
                      added_points_pyramid_wetland, added_points_pyramid_upland, 
                      added_points_tumtum_wetland, added_points_tumtum_openwater)

# Combine with previous training points
all_points <- rbind(raw_and_added, added_points)

# Ensure class is factor immediately after combining
all_points$class <- as.factor(all_points$class)

all_points$site[all_points$site == "tum_tum"] <- "tumtum" # glitch

cat("\n--- All Points Summary ---\n")
cat("Total points:", nrow(all_points), "\n")
cat("Class type:", class(all_points$class), "\n")
cat("Levels:", paste(levels(all_points$class), collapse = ", "), "\n\n")

summary_2 <- all_points %>%
  group_by(site, class) %>%
  summarise(count = n(), .groups = "drop")
print(summary_2)

# Save the final cleaned point data for efficient loading in other scripts
saveRDS(all_points, here("outputs/01_cleaneddata/all_points.rds"))


