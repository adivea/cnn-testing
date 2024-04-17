## Quick routine to get the bounding boxes for mounds

###################################### LOAD MOUND DATA
library(tidyverse)
library(sf)
library(raster)

# Bring in all the mounds
mounds <- st_read("../1_Teaching/cds-spatial/data/KAZ_mounds.shp")
mounddata <- read_csv("data/KAZ_moundAttributes.csv")
# filter the noticeable ones (2+ meters)
mounds <- mounds %>% 
  left_join(mounddata, by = c("TRAP_Code"="TRAP_ID"))

##################################### CREATE RADIUS


# For mound bounding boxes use the line below

# Variable radius
radius <- (mounds$Length+0.25*mounds$Length)/2 

# Set radius
# radius <- (mounds$Length+10)/2 


mounds$northing <- st_coordinates(mounds)[,2]
mounds$easting <- st_coordinates(mounds)[,1]


# define the plot edges based upon the plot radius. 

yPlus <- mounds$northing+radius
xPlus <- mounds$easting+radius
yMinus <- mounds$northing-radius
xMinus <- mounds$easting-radius

# calculate polygon coordinates for each plot centroid. 
square <- cbind(xMinus,yPlus,  # NW corner
                xPlus, yPlus,  # NE corner
                xPlus,yMinus,  # SE corner
                xMinus,yMinus, # SW corner
                xMinus,yPlus)  # NW corner again - close polygon

# Extract the mound ID information
ID <- mounds$TRAP_Code


############################################# GENERATE BOUNDING BOXES

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
polys_df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
polys_df
mnd_poly <- st_as_sf(polys_df)
mnd_poly



# Add attributes
mnd_poly <- mnd_poly %>% 
  left_join(mounddata, by = c("id"="TRAP_ID"))



############################################ VIEW BBOXES

library(mapview)
mapview(mnd_poly, zcol= "Length")



############################################ EXPORT BBOXES

st_write(mnd_poly, "data/Kaz_mndbbox.geojson")
