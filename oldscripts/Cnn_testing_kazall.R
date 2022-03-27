## Testing results of CNN predictions on merged image

library(tidyverse)
library(sf)
library(raster)


####################################### LOAD DATA

# Load the merged Satellite image for Kazanlak Valley, Bulgaria
kaz <-brick("../1_Teaching/cds-spatial/data/Kaz.tif")
plotRGB(kaz, stretch= "lin")
crs(kaz)

# Load prediction data as points (left bottom corner of the evaluated cell)
cnne_df <- read_csv("2021-10-25.predictions/results/east/east.csv")
# 15334 points (origins in the raster cells)
cnnw_df <- read_csv("2021-10-25.predictions/results/west/west.csv")
# 15334 points (origins in the raster cells)
cnn_df <- rbind(cnne_df, cnnw_df)
# 30504 rows

# Check the probabilities of there being a mound in a given cell
hist(cnn_df$mound_probability)
table(cut(cnn_df$mound_probability, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))

# ok, so there is an exponential drop-off. 

##################################################################
### Make these predictions spatial 
### and build a grid of those with 60%+ probability of containing a mound

# Take all points, convert to spatial points and then to grid
cnnall_sp <- st_as_sf(cnn_df, coords = c("x","y"), crs = 32635)
cnnall_grid <- st_make_grid(cnnall_sp, cellsize = 250, what = "polygons")


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

################################### VALIDATE AGAINST FIELD DATA

# Bring in survey area
survey <- st_read("../1_Teaching/cds-spatial/data/KAZ_surveyarea.shp")
#survey_sm <- st_simplify(survey, dTolerance = 250)
plot(survey$geometry)

# Bring in all the mounds observed in the field
mounds <- st_read("../1_Teaching/cds-spatial/data/KAZ_mounds.shp")

# View all mounds
plotRGB(kaze, stretch= "lin");
plot(survey$geometry, add = TRUE, col = "lightyellow", );
plot(mounds$geometry, add = TRUE, col = "pink");
plot(cnn_grid, add = TRUE, border = "white")


# Bring in mound attributes (n = 773)
mounddata <- read_csv("../1_Teaching/cds-spatial/data/KAZ_mdata.csv")
# filter the noticeable ones (2+ meters)
large <- mounddata %>% 
  filter(Height>=2) %>%  # ~250
  left_join(mounds, by = c("MoundID"="TRAP_Code"))

# View Large mounds only
plotRGB(kaze, stretch= "lin");
plot(mounds$geometry, add = TRUE, col = "red")
plot(large$geometry, add = TRUE, col = "yellow")
plot(cnn_grid, add = TRUE, border = "white")


##################### EVALUATION

# ok, several clusters are missing: e.g. NW necropolis and northern
# royal mounds as well as a few individual large mounds (e.g. east-most peninsula)
# false positives dominate south of the reservoir


# Visualise overall grid probability vs the actual mounds
cnn_datagrid %>% 
  filter(mound_probability>0.5) %>% 
  ggplot()+
  geom_sf(aes(color = mound_probability))+
  scale_color_gradient(low = "green", 
                       high = "red",
                       "Mound probability")+
  geom_sf(data = large$geometry, aes(size = large$Height, alpha = 0.01))+
  geom_sf(data = large_overlap)+
 # geom_sf(data = survey$geometry)+
  ggtitle("Kazanlak")


# Export data
st_write(cnn_datagrid, "data/Cnn_datagrid.shp")


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
