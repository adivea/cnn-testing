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
# kaz <-brick("../1_Teaching/cds-spatial-2022/data/Kaz.tif")
# plotRGB(kaz, stretch= "lin")
# crs(kaz)

# # East first
# kaze <-brick("../1_Teaching/SpatialAnalytics2021/scripts_rs/data/KazE.tif")
# plotRGB(kaze, stretch= "lin")
# 
# # West next
# kazw <-brick("../1_Teaching/SpatialAnalytics2021/scripts_rs/data/KazW.tif")
# plotRGB(kazw, stretch= "hist")


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
# 15170 points (origins in the raster cells)

# Datasets have repeating numeric ID, add 100000 to differentiate West for later poly creation
cnnw_df[,1] <- cnnw_df[,1] +100000 

# Combine the prediction data from E and W half into one dataset
cnn_df <- rbind(cnne_df, cnnw_df)     # # 30504 rows, combined east and west (overlap!)
names(cnn_df)[1] <- "Image filename"

# Look at the distribution of the predictions
hist(cnn_df$mound_probability, main = "Probability of a mound") 
which(is.na(cnn_df$mound_probability))

#######################################LOAD MOSAICED SATELLITE IMAGES
# data for this section was generated via the MOSAIC.R script

# # Need hi-res mosaiced but cropped raster?
# # If large raster is not in memory
# KAZcropped <- brick("E:/TRAP Workstation/Shared GIS/Satellite imagery/IKONOS/Kazanlak/ERDAS/Cormac/Kazcropped.tif")

# # Need hi-res mosaiced and adjusted raster to cover 100m of mounds?
kaz <- brick("G:/TRAP Workstation/Shared GIS/Satellite imagery/IKONOS/Kazanlak/ERDAS/Cormac/Kazcrop_adj.tif")


#######################################LOAD and TRANSFORM PREDICTIONS

# Look at the distribution of the 'corrected' predictions
hist(1-cnn_df$mound_probability, main = "Probability of a mound") 
which(is.na(1-cnn_df$mound_probability))

# Create a grid of ALL the predictions to see where mounds are in relation to it (overlap!)
# cnnall_sp <- st_as_sf(cnn_df, coords = c("x","y"), crs = 32635)
# cnnall_grid <- st_make_grid(cnnall_sp, cellsize = c(150,150), what = "polygons")
# cnnall_grid <- st_join(st_sf(cnnall_grid), cnnall_sp) # add attributes does not work as four points are at edges
# mapview(cnnall_grid)+mapview(cnnall_sp)

##################################### SPATIAL GRIDS OUT OF PREDICTION 60% thresholds

### 60 THRESHOLD: Build a grid of those cells with 60%+ probability of containing a mound

# Filter predictions to those that have 60+% likelihood of containing a mound
# and make into points
cnn60_pt <- cnn_df %>% 
  mutate(mound_probability = 1 - cnn_df$mound_probability) %>%  # in 2022, prob refers to not-mound, so inverting 
  filter(mound_probability > 0.59) %>% # 897 observations have 60+% proabbility of being mounds (very close to original)
  st_as_sf(coords = c("x","y"), crs = 32635)



################################ GENERATE 150m GRIDS MANUALLY

side <- 150 # stamps are 150m per side of polygon


######################### GRID MAKING MANUAL for 60%+ AREAS
# create a df with only 60%+ probability, inverting the provided probability
cnn60_df <- cnn_df %>% 
  mutate(mound_probability = 1 - cnn_df$mound_probability) %>%  # Ross suggests mound_prob field refers to not-mound, so inverting 
  filter(mound_probability > 0.599)

y <- cnn60_df$y
x <- cnn60_df$x


# define the plot edges based upon the plot radius. 
yPlus <- y+side
xPlus <- x+side

# calculate polygon coordinates for each plot centroid. 
square <- cbind(x,yPlus,  # NW corner
                xPlus, yPlus,  # NE corner
                xPlus,y,  # SE corner
                x,y, # SW corner
                x,yPlus)  # NW corner again - close polygon

# Extract the image ID information
ID <- cnn60_df$"Image filename"

# create spatial polygons (squares) from mound coordinates with mapply
polys <- SpatialPolygons(mapply(function(poly, id) 
{
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, 
split(square, row(square)), ID),
proj4string=CRS(as.character("+proj=utm +zone=35 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

plot(polys)

# Convert to sf feature via a SpatialDataframe (to preserve mound IDs)
polys_df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names = ID,
                                                       mound_probability = cnn60_df$mound_probability))
polys_df
grid60 <- st_as_sf(polys_df)

# Write to shapefile
# st_write(grid60, "output_data/grid60_150.shp",driver = 'ESRI Shapefile', append=FALSE)

# Clean up interim files
remove(polys,polys_df, square, x,y,xPlus,yPlus,side)


# Look at the polygons
library(mapview)
mapview(grid60)+mapview(cnn60_pt)

# Visualize the grid cells with higher probability 
ggplot(grid60) +
  geom_sf(aes(color = mound_probability))

##################################### SPATIAL GRIDS OUT OF PREDICTION 80% thresholds

# Filter predictions to those that have 80+% likelihood of containing a mound
cnn80_pt <- cnn_df %>% 
  mutate(mound_probability = 1 - cnn_df$mound_probability) %>%  # in 2022, prob refers to not-mound, so inverting 
  filter(mound_probability >0.799) %>%  # 78 observations
  st_as_sf(coords = c("x","y"), crs = 32635)


############################# GRID MAKING MANUAL 80%
# create a df with only 80%+ probability, inverting the provided probability
cnn80_df <- cnn_df %>% 
  mutate(mound_probability = 1 - cnn_df$mound_probability) %>%  # in 2022, prob refers to not-mound, so inverting 
  filter(mound_probability > 0.799)

side <- 150 #twice that is 150m per side of polygon

y <- cnn80_df$y
x <- cnn80_df$x


# define the plot edges based upon the plot radius. 
yPlus <- y+side
xPlus <- x+side

# calculate polygon coordinates for each plot centroid. 
square <- cbind(x,yPlus,  # NW corner
                xPlus, yPlus,  # NE corner
                xPlus,y,  # SE corner
                x,y, # SW corner
                x,yPlus)  # NW corner again - close polygon

# Extract the image ID information
ID <- cnn80_df$`Image filename`

# create spatial polygons (squares) from mound coordinates with mapply
polys <- SpatialPolygons(mapply(function(poly, id) 
{
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, 
split(square, row(square)), ID),
proj4string=CRS(as.character("+proj=utm +zone=35 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

plot(polys)

# Convert to sf feature via a SpatialDataframe (to preserve mound IDs)
polys_df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names = ID,
                                                       mound_probability = cnn80_df$mound_probability))
polys_df
grid80 <- st_as_sf(polys_df)

# Write to shapefile
# st_write(grid60, "output_data/grid60_150.shp",driver = 'ESRI Shapefile', append=FALSE)

# Clean up interim files
remove(polys,polys_df, square, x,y,xPlus,yPlus,side)

# Look at the polygons
mapview(grid80)+mapview(cnn80_pt)

# Nearly everything is predicted to be a mound

################################### BRING IN FIELD DATA


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

################################    GET MOUND DATA
# Bring in all the mounds
mounds <- st_read("../1_Teaching/cds-spatial-2022/data/KAZ_mounds.shp")
mounddata <- read_csv("../1_Teaching/cds-spatial-2022/data/KAZ_mdata.csv")
# filter the noticeable ones (2+ meters)
mounds <- mounds %>% 
  left_join(mounddata, by = c("TRAP_Code"="MoundID"))

##################################### INTERSECT WITH STUDY AREA


### Crop 60%+ and 80% cell grid to TRAP study area
# Let's see which of the grid cells with high probability of containing a mound are in the TRAP study area?

survey_grid60 <- st_intersection(survey_ch, grid60) # 9816 grid cells with 60%+
survey_grid80 <- st_intersection(survey_ch, grid80) # 7260 grid cells with 80%+


