library(tidyverse)
library(sf)
library(raster)
####################################### EAST

# East first
kaze <-brick("../1_Teaching/cds-spatial/data/Kaz.tif")
plotRGB(kaze, stretch= "lin")
crs(kaze)

# 
cnne_df <- read_csv("2021-10-25.predictions/results/east/east.csv")
# 15324 points (origins in the raster cells)
cnne <- st_as_sf(cnne_df, coords = c("x","y"), crs = 32635)
# Let's filter to those that have 60+% likelihood of containing a mound
cnne_sp <- cnne_df %>% 
  filter(mound_probability > 0.60) %>% 
  st_as_sf(coords = c("x","y"), crs = 32635)
# only 146 raster cells are predicted to contain mounds with greater than 60% likelihood

# Visualize the grid cells with higher probability 
plotRGB(kaze, stretch= "lin");
plot(cnne_sp$geometry, add = TRUE, col = "red")

ggplot(cnne_grid) +
  geom_sf(aes(color = mound_probability))

# Make grids
cnne_grid <- st_make_grid(cnne, cellsize = 250, what = "polygons")
plot(cnne_grid[cnne_sp])

# View the grid of 60% mounds
plotRGB(kaze, stretch= "lin");
#plot(cnne_sp$geometry, add = TRUE, col = "red");
plot(cnne_grid[cnne_sp], add = TRUE, border = "white")

st_bbox(kaze)

### Validation
# Bring in all the mounds
mounds <- st_read("data/KAZ_mounds.shp")

plotRGB(kaze, stretch= "lin");
plot(cnn_grid[cnne_sp], add = TRUE, border = "white")
plot(mounds$geometry, add = TRUE, col = "red")

# ok, a number of mounds are outside the grids, 
# with most surprising absentee being the east-most peninsula

################################################ WEST

### Let's test the West

kazw <-brick("data/KazW.tif")
plotRGB(kazw, stretch= "hist")
crs(kazw)

# Load CNN data 
cnnw_df <- read_csv("../../../CNN_ross/2021-10-25.predictions/results/west/west.csv")
# 15324 points (origins in the raster cells)
cnnw <- st_as_sf(cnnw_df, coords = c("x","y"), crs = 32635)
# Let's filter to those that have 60+% likelihood of containing a mound
cnnw_sp <- cnnw_df %>% 
  filter(mound_probability > 0.59) %>%  #169
  st_as_sf(coords = c("x","y"), crs = 32635)
# only 147 raster cells are predicted to contain mounds with greater than 60% likelihood
# the number rises to 169 with p>0.59

# Visualize the 169
plotRGB(kazw, stretch= "lin");
plot(cnnw_sp$geometry, add = TRUE, col = "red")


# Make grids
cnnw_grid <- st_make_grid(cnnw,cellsize = 250,what = "polygons")
plot(cnnw_grid[cnnw_sp])

# View the grid of 60% mounds and identified mounds
plotRGB(kazw, stretch = "lin", interpolate = TRUE, colNA= 'white');
plot(cnnw_grid[cnnw_sp], add = TRUE, border = "white");
plot(mounds$geometry, add = TRUE, col = "red")

############################### Play with grids
# Combine grids
kaz_grid <- c(cnnw_grid[cnnw_sp], cnne_grid[cnne_sp])

# plotRGB(kazw, stretch = "lin", interpolate = TRUE, colNA= 'white');
# plotRGB(kaze, stretch = "lin", interpolate = TRUE, colNA= 'white');
plot(mounds$geometry, col = "red");
plot(kaz_grid, add = TRUE)


# Check overlap
test <- st_intersection(mounds, kaz_grid) # 161 mounds appear in the 380 grids
test
plot(test$geometry)


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
