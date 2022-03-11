##### This script loads all the data necessary for mound validation
# including both individual and composite adjusted Kaz valley rasters
# including mounds and survey area boundary and grids
# Some of this data is redundant (depending on analysis), and so it's recomended to 
# pick the data you need or face high memory take

library(tidyverse)
library(sf)
library(raster)

#######################################LOAD SMALLER SATELLITE IMAGE

# Load the merged Satellite image for Kazanlak Valley, Bulgaria
# use cds-spatial repo from github https://github.com/CDS-AU-DK/cds-spatial-2022
kaz <-brick("../1_Teaching/cds-spatial-2022/data/Kaz.tif")
plotRGB(kaz, stretch= "lin")
crs(kaz)

# East first
kaze <-brick("../1_Teaching/SpatialAnalytics2021/scripts_rs/data/KazE.tif")
plotRGB(kaze, stretch= "lin")

# West next
kazw <-brick("../1_Teaching/SpatialAnalytics2021/scripts_rs/data/KazW.tif")
plotRGB(kazw, stretch= "hist")


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
  mutate(inv_mound_probability = 1 - cnn_df$mound_probability) %>%  # in 2022, prob refers to not-mound, so inverting 
  filter(inv_mound_probability > 0.59) %>% # 
   # filter(mound_probability > 0.59) %>%  #333 observations
   st_as_sf(coords = c("x","y"), crs = 32635)

# Make a grid of 60%+ cells, 382 cells, 332 unique ones
cnn_grid60 <- st_make_grid(cnn60_sp, cellsize = 250, what = "polygons")
#plot(cnn_grid60)

# Re-add probability data to the 60%+ grid, as the gridmaking stripped the values out
cnn_grid60 <- st_join(st_sf(cnn_grid60), cnn60_sp)

# Visualize the grid cells with higher probability 
ggplot(cnn_grid60) +
  geom_sf(aes(color = mound_probability))

# 333 raster cells are predicted to contain mounds with greater 
# than 60% likelihood. Archaeologists found 773 mounds in fraction of the area

##################################### 

# Bring in survey area to see overall coverage
survey <- st_read("../1_Teaching/cds-spatial-2021/data/KAZ_surveyarea.shp")
plot(survey$geometry, main = "Area covered by survey")

# convex hull of survey polygons 
survey_ch <- st_convex_hull(st_union(survey$geometry))

# # Extrapolate rough study area after subtracting outliers
# far <- mounds %>% 
#   st_is_within_distance(survey, 1500) %>% 
#   lengths>0 
# farmounds <- which(far==0)  # these are the mounds that are mostly far from survey area
# 
# # convex hull of points without outliers
# survey_sm <- st_convex_hull(st_union(mounds$geometry[-farmounds]))

##################################### SPATIAL GRIDS OUT OF PREDICTION 80% thresholds

# Filter predictions to those that have 80+% likelihood of containing a mound
cnn80_sp <- cnn_df %>% 
  filter(mound_probability >0.799) %>%  #333 observations
  st_as_sf(coords = c("x","y"), crs = 32635)

# Make a grid of 80%+ cells, 382 cells, 332 unique ones
cnn_grid80 <- st_make_grid(cnn80_sp, cellsize = 250, what = "polygons")
#plot(cnn_grid80)

# Add probability data to the 80%+ grid 
#cnnall_datagrid <- st_join(st_sf(cnnall_grid), cnnall_sp)
cnn_grid80 <- st_join(st_sf(cnn_grid80), cnn80_sp)

# Visualize the grid cells with higher probability 
ggplot(cnn_grid80) +
  geom_sf(aes(color = mound_probability))


##################################### INTERSECT WITH STUDY AREA


### Crop 60%+ and 80% cell grid to TRAP study area
# Let's see which of the grid cells with high probability of containing a mound are in the TRAP study area?

survey_grids60 <- st_intersection(survey_ch, cnn_grid60) # 192 grid cells with 60%+
survey_grids80 <- st_intersection(survey_ch, cnn_grid80) # 40 grid cells with 80%+


####################################### HALF AND HALF APPROACH

################################# EAST ONLY 


cnne <- st_as_sf(cnne_df, coords = c("x","y"), crs = 32635)
# Filter EAST predictions to those that have 60+% likelihood of containing a mound
cnne_sp6 <- cnne_df %>% 
  filter(mound_probability > 0.60) %>% 
  st_as_sf(coords = c("x","y"), crs = 32635)
# only 146 raster cells are predicted to contain mounds with greater than 60% likelihood

# Filter EAST predictions to those that have 80+% likelihood of containing a mound
cnne_sp8 <- cnne_df %>% 
  filter(mound_probability > 0.80) %>% 
  st_as_sf(coords = c("x","y"), crs = 32635)
# only 146 raster cells are predicted to contain mounds with greater than 60% likelihood

# Make EAST grids
cnne_grid <- st_make_grid(cnne, cellsize = 250, what = "polygons")

# View the grid of 60% mounds
plotRGB(kaze, stretch= "lin");
plot(cnne_grid[cnne_sp6], add = TRUE, border = "white")
plot(cnne_grid[cnne_sp8], add = TRUE, border = "pink")

#################################  WEST ONLY 

cnnw <- st_as_sf(cnnw_df, coords = c("x","y"), crs = 32635)
# Filter WEST to those that have 60+% likelihood of containing a mound
cnnw_sp6 <- cnnw_df %>% 
  filter(mound_probability > 0.60) %>%  #169
  st_as_sf(coords = c("x","y"), crs = 32635)
# only 147 raster cells are predicted to contain mounds with greater than 60% likelihood
# the number rises to 169 with p>0.59

# Filter WEST to those that have 80+% likelihood of containing a mound
cnnw_sp8 <- cnnw_df %>% 
  filter(mound_probability > 0.80) %>%  #169
  st_as_sf(coords = c("x","y"), crs = 32635)

# Make WEST grids
cnnw_grid <- st_make_grid(cnnw,cellsize = 250,what = "polygons")

# View the grid of 60% mounds and identified mounds
plotRGB(kazw, stretch = "lin", interpolate = TRUE, colNA= 'white');
plot(cnnw_grid[cnnw_sp6], add = TRUE, border = "white");
plot(cnnw_grid[cnnw_sp6], add = TRUE, border = "pink");

################################    GET MOUND DATA
# Bring in all the mounds
mounds <- st_read("../1_Teaching/cds-spatial-2022/data/KAZ_mounds.shp")
mounddata <- read_csv("../1_Teaching/cds-spatial-2022/data/KAZ_mdata.csv")
# filter the noticeable ones (2+ meters)
mounds <- mounds %>% 
  left_join(mounddata, by = c("TRAP_Code"="MoundID"))

# ok, a number of mounds are outside the grids, 
# with most surprising absentee being the east-most peninsula

############################################## # Combine grids


# Insert data into grids
cnne_datagrid <- st_join(st_sf(cnne_grid), cnne)
cnnw_datagrid <- st_join(st_sf(cnnw_grid), cnnw)


# Combine into master-grid
cnn_datagrid <- rbind(cnnw_datagrid,cnne_datagrid)


# Visualise overall grid probability vs the actual mounds
# Legend label editing: https://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot 
cnn_datagrid %>% 
  filter(mound_probability>0.5) %>% 
  ggplot()+
  geom_sf(aes(color = mound_probability))+
  scale_color_gradient(low = "green", 
                       high = "red",
                       "Mound probability")+
  theme(legend.position = "bottom")+
  theme_bw()+
  geom_sf(data = mounds, aes(size = Height), alpha = 0.4)+
  labs(size = "Mound height (m)")+
  ggtitle("Kazanlak Mound Predictions and Field Data")
  


# Export data
dir.create("output_data")
st_write(cnn_datagrid, "output_data/Cnn_datagrid.shp")
st_write(mounds, "output_data/mounds.shp")
