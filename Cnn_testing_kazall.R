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


# Filter to those that have 60+% likelihood of containing a mound
cnn60_sp <- cnn_df %>% 
  filter(mound_probability > 0.59) %>%  #333 observations
  st_as_sf(coords = c("x","y"), crs = 32635)

# 333 raster cells are predicted to contain mounds with greater 
# than 60% likelihood. Archaeologists found 773 mounds in fraction of the area

# Visualize the grid cells with higher probability 
plotRGB(kaz, stretch= "lin");
plot(cnn60_sp$geometry, add = TRUE, col = "white")

ggplot(cnn_grid) +
  geom_sf(aes(color = mound_probability))

# Make a grid of 60%+ cells
cnn_grid <- st_make_grid(cnn60_sp, cellsize = 250, what = "polygons")
plot(cnn_grid)

# View the grid 
plotRGB(kaze, stretch= "lin");
#plot(cnn_sp$geometry, add = TRUE, col = "red");
plot(cnn_grid, add = TRUE, border = "white")


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



############## VISUALIZE OVERLAP

# Ensure the whole and 60%+ grid contains mound probability info from original prediction
#cnnall_datagrid <- st_join(st_sf(cnnall_grid), cnnall_sp)
cnn_datagrid <- st_join(st_sf(cnn_grid), cnn60_sp)

# Check spatial overlap between 60%+ grid cells and all 773 mounds 
overlap <- st_intersection(mounds, cnn_grid) # 146 mounds appear in the 380 grids
overlap #146 features here
plot(overlap$geometry)
st_difference(overlap$geometry, mounds$geometry)

# Check for intersection between mounds of 2+m height and grids with 60%+ probability
large_overlap <- st_intersection(cnn_datagrid$geometry, large$geometry)
plot(st_as_sf(large_overlap))  # 37 mounds are captured, this is a list of the

d
# See the 146 mound features of all sizes 
cnn_datagrid %>% 
 # filter(mound_probability>0.5) %>% 
  ggplot()+
  geom_sf(aes(color = mound_probability))+
  scale_color_gradient(low = "green", 
                       high = "red",
                       "Mound probability")+
  geom_sf(data = test$geometry)+
 # geom_sf(data = large_overlap)+
  # geom_sf(data = survey$geometry)+
  ggtitle("Kazanlak")

# See the 37 2+m high mounds 
cnn_datagrid %>% 
  # filter(mound_probability>0.5) %>% 
  ggplot()+
  geom_sf(aes(color = mound_probability))+
  scale_color_gradient(low = "green", 
                       high = "red",
                       "Mound probability")+
  #geom_sf(data = test$geometry)+
   geom_sf(data = large_overlap)+
  # geom_sf(data = survey$geometry)+
  ggtitle("Kazanlak")

# See the missing/unpredicted mound locations and their size
?st_difference()
test <- st_intersects(cnn_datagrid$geometry, mounds$geometry)

missing <- st_difference(cnn_datagrid$geometry, mounds$geometry)
missing %>% 
  group_by(TRAP_Code) %>% 
  tally()





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

##### RANDOM OTHER STUFF


