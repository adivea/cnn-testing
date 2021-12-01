library(tidyverse)
library(sf)
library(raster)

#######################################LOAD SATELLITE IMAGE

# Load the merged Satellite image for Kazanlak Valley, Bulgaria
# use cds-spatial repo from github https://github.com/CDS-AU-DK/cds-spatial
kaz <-brick("../1_Teaching/cds-spatial/data/Kaz.tif")
plotRGB(kaz, stretch= "lin")
crs(kaz)

# East first
kaze <-brick("../1_Teaching/SpatialAnalytics2021/scripts_rs/data/KazE.tif")
plotRGB(kaze, stretch= "lin")

# West next
kazw <-brick("../1_Teaching/SpatialAnalytics2021/scripts_rs/data/KazW.tif")
plotRGB(kazw, stretch= "hist")

#######################################LOAD PREDICTIONS

# Load prediction data as points (left bottom corner of the evaluated cell)
cnne_df <- read_csv("2021-10-25.predictions/results/east/east.csv")  # eastern half
# 15334 points (origins in the raster cells)
cnnw_df <- read_csv("2021-10-25.predictions/results/west/west.csv")  # western half
# 15334 points (origins in the raster cells)
cnn_df <- rbind(cnne_df, cnnw_df)     # # 30504 rows, combined east and west (overlap!)


# Create a grid of ALL the predictions to see where mounds are in relation to it (overlap!)
cnnall_sp <- st_as_sf(cnn_df, coords = c("x","y"), crs = 32635)
cnnall_grid <- st_make_grid(cnnall_sp, cellsize = 250, what = "polygons") 
cnnall_grid <- st_join(st_sf(cnnall_grid), cnnall_sp) # add attributes


##################################### SPATIAL GRIDS OUT OF PREDICTION 60% thresholds

### 60 THRESHOLD: Build a grid of those cells with 60%+ probability of containing a mound

# Filter predictions to those that have 60+% likelihood of containing a mound
cnn60_sp <- cnn_df %>% 
  filter(mound_probability > 0.59) %>%  #333 observations
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


# Bring in all the mounds
mounds <- st_read("data/KAZ_mounds.shp")


# ok, a number of mounds are outside the grids, 
# with most surprising absentee being the east-most peninsula

############################################## # Combine grids


# Insert data into grids
cnne_datagrid <- st_join(st_sf(cnne_grid), cnne)
cnnw_datagrid <- st_join(st_sf(cnnw_grid), cnnw)


# Combine into master-grid
cnn_datagrid <- rbind(cnnw_datagrid,cnne_datagrid)


# Visualise overall grid probability vs the actual mounds
cnn_datagrid %>% 
  filter(mound_probability>0.5) %>% 
  ggplot()+
  geom_sf(aes(color = mound_probability))+
  scale_color_gradient(low = "green", 
                       high = "red",
                       "Mound probability")+
  geom_sf(data = mounds, aes(alpha = 0.01))+
  ggtitle("Kazanlak East")


# Export data
st_write(cnn_datagrid, "data/Cnn_datagrid.shp")
