###################################### LOAD MOUND DATA
library(tidyverse)
library(sf)
library(raster)

# Bring in all the mounds
mounds <- st_read("../1_Teaching/cds-spatial/data/KAZ_mounds.shp")
mounddata <- read_csv("../1_Teaching/cds-spatial/data/KAZ_mdata.csv")
# filter the noticeable ones (2+ meters)
mounds <- mounds %>% 
  left_join(mounddata, by = c("TRAP_Code"="MoundID"))

####################################### LOAD SMALL KAZ IKONOS
kaz <-brick("../1_Teaching/cds-spatial/data/Kaz.tif")

####################################### LOAD HI-RES CROPPED KAZ IKONOS

# Need hi-res cropped raster? This one just encompasses the burial mounds with a 100
KAZcropped <- brick("E:/TRAP Workstation/Shared GIS/Satellite imagery/IKONOS/Kazanlak/ERDAS/Cormac/Kazcrop_adj.tif")

