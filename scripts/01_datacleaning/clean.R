library(sf)
library(terra)
library(dplyr)
library(writexl)

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

points_all_cleaned <- st_zm(points_all_cleaned, what = "ZM")

# I want to know the total number of points by site and class
summary <- points_all_cleaned %>%
  group_by(Site, Code) %>%
  summarise(count = n(), .groups = "drop")

# Save point files
save(points_all_cleaned, file = "/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/outputs/01_cleaneddata/points_all_cleaned.RData")
write_xlsx(points_all_cleaned, "/Users/nhile/Documents/WetlandModel/wetland-ml-classifier/outputs/01_cleaneddata/points_all_cleaned.xlsx")

