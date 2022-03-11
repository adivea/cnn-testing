##### This script loads all the data necessary for mound validation
# including both individual and composite adjusted Kaz valley rasters
# including mounds and survey area boundary and grids
# Some of this data is redundant (depending on analysis), and so it's recomended to 
# pick the data you need or face high memory take

library(tidyverse)
library(sf)
library(raster)

#######################################LOAD SMALLER SATELLITE IMAGE

# # Load the merged Satellite image for Kazanlak Valley, Bulgaria
# # use cds-spatial repo from github https://github.com/CDS-AU-DK/cds-spatial-2022
# kaz <-brick("../1_Teaching/cds-spatial-2022/data/Kaz.tif")
# plotRGB(kaz, stretch= "lin")
# crs(kaz)
# 
# # East first
# kaze <-brick("../1_Teaching/SpatialAnalytics2021/scripts_rs/data/KazE.tif")
# plotRGB(kaze, stretch= "lin")
# 
# # West next
# kazw <-brick("../1_Teaching/SpatialAnalytics2021/scripts_rs/data/KazW.tif")
# plotRGB(kazw, stretch= "hist")
# 

#######################################LOAD MOSAICED SATELLITE IMAGES
# data for this section was generated via the MOSAIC.R script

# # Need hi-res mosaiced but cropped raster?
# # If large raster is not in memory
# KAZcropped <- brick("E:/TRAP Workstation/Shared GIS/Satellite imagery/IKONOS/Kazanlak/ERDAS/Cormac/Kazcropped.tif")

# # Need hi-res mosaiced and adjusted raster to cover 100m of mounds?
kaz <- brick("G:/TRAP Workstation/Shared GIS/Satellite imagery/IKONOS/Kazanlak/ERDAS/Cormac/Kazcropped.tif")


#######################################LOAD PREDICTIONS

# Load prediction data as points (left bottom corner of the evaluated cell)
cnne_df <- read_csv("2021-10-25_predictions/results/east/east.csv")  # eastern half
# 15334 points (origins in the raster cells)
cnnw_df <- read_csv("2021-10-25_predictions/results/west/west.csv")  # western half
# 15334 points (origins in the raster cells)


# Combine the prediction data from E and W half into one dataset
cnn_df <- rbind(cnne_df, cnnw_df)     # # 30504 rows, combined east and west (overlap!)
cnn_df <- read_csv("2022-03-10_predictions/2021-03-10.Predictions.csv")

# Look at the distributionof the predictions
hist(1-cnn_df$`Raw Prediction`, main = "Probability of a mound") 
which(is.na(1-cnn_df$`Raw Prediction`))

# Create a grid of ALL the predictions to see where mounds are in relation to it (overlap!)
# cnnall_sp <- st_as_sf(cnn_df, coords = c("coord_x","coord_y"), crs = 32635)
# cnnall_grid <- st_make_grid(cnnall_sp, cellsize = 250, what = "polygons") 
# cnnall_grid <- st_join(st_sf(cnnall_grid), cnnall_sp) # add attributes


##################################### SPATIAL GRIDS OUT OF PREDICTION 60% thresholds

### 60 THRESHOLD: Build a grid of those cells with 60%+ probability of containing a mound

# Filter predictions to those that have 60+% likelihood of containing a mound
cnn60_sp <- cnn_df %>% 
  mutate(mound_probability = 1 - cnn_df$`Raw Prediction`) %>%  # in 2022, prob refers to not-mound, so inverting 
  filter(mound_probability > 0.59) %>% # 897 observations have 60+% proabbility of being mounds (very close to original)
  st_as_sf(coords = c("coord_x","coord_y"), crs = 32635)


# Make a grid of 60%+ cells, 897 cells, all unique
cnn_grid60 <- st_make_grid(cnn60_sp, cellsize = 250, what = "polygons")

# Re-add probability data to the 60%+ grid, as the gridmaking stripped the values out
cnn_grid60 <- st_join(st_sf(cnn_grid60), cnn60_sp)

# Visualize the grid cells with higher probability 
ggplot(cnn_grid60) +
  geom_sf(aes(color = mound_probability))

# 897 raster cells are predicted to contain mounds with greater 
# than 60% likelihood. Archaeologists found 773 mounds in 
# quarter of the area, so there is potential for 25% overlap here.


##################################### SPATIAL GRIDS OUT OF PREDICTION 80% thresholds

# Filter predictions to those that have 80+% likelihood of containing a mound
cnn80_sp <- cnn_df %>% 
  mutate(mound_probability = 1 - cnn_df$`Raw Prediction`) %>%  # in 2022, prob refers to not-mound, so inverting 
  filter(mound_probability >0.799) %>%  # 78 observations
  st_as_sf(coords = c("coord_x","coord_y"), crs = 32635)

# Make a grid of 80%+ cells, 78 cells
cnn_grid80 <- st_make_grid(cnn80_sp, cellsize = 250, what = "polygons")
#plot(cnn_grid80)

# Add probability data to the 80%+ grid 
cnn_grid80 <- st_join(st_sf(cnn_grid80), cnn80_sp)

# Visualize the grid cells with higher probability 
ggplot(cnn_grid80) +
  geom_sf(aes(color = mound_probability))

############################## FIELD SURVEY DATA FOR VALIDATION
# Bring in mounds
# Bring in all the mounds
mounds <- st_read("../1_Teaching/cds-spatial-2022/data/KAZ_mounds.shp")
mounddata <- read_csv("../1_Teaching/cds-spatial-2022/data/KAZ_mdata.csv")

# join attributes to shapefile
mounds <- mounds %>% 
  left_join(mounddata, by = c("TRAP_Code"="MoundID"))


# Bring in survey area to see overall coverage
survey <- st_read("../1_Teaching/cds-spatial-2021/data/KAZ_surveyarea.shp")
plot(survey$geometry, main = "Area covered by survey")

# convex hull of survey polygons 
survey_ch <- st_convex_hull(st_union(survey$geometry))

# Extrapolate rough study area after subtracting outliers
far <- mounds %>%
  st_is_within_distance(survey, 1500) %>%
  lengths>0
farmounds <- which(far==0)  # these are the mounds that are mostly far from survey area

# convex hull of points without outliers
survey_sm <- st_convex_hull(st_union(mounds$geometry[-farmounds]))

rm(far, farmounds)

#############################INTERSECT GRIDS WITH STUDY AREA


### Crop 60%+ and 80% cell grid to TRAP study area
# Let's see which of the grid cells with high probability of containing a mound are in the TRAP study area?

survey_grids60 <- st_intersection(survey_ch, cnn_grid60) # 192 grid cells with 60%+
survey_grids80 <- st_intersection(survey_ch, cnn_grid80) # 40 grid cells with 80%+


