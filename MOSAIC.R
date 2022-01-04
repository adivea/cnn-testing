################################ MOSAIC ORIGINAL SATELLITE IMAGES - LARGE!!
# This script was used to create background image for cutout mound stamps for CNN


library(easypackages)
libraries("rgdal", "gdalUtils", "raster")
#setwd("E:/TRAP Workstation/Shared GIS/Satellite imagery/IKONOS/Kazanlak/ERDAS/Cormac")

# Create an extent for mounds
extent <- st_make_grid(st_bbox(visiblemounds3_5),n = 1)

# Load original  Kaz_e_fuse and Kaz_w_fuse from E:\TRAP Workstation\Shared GIS\Satellite imagery\IKONOS\Kazanlak\ERDAS\Cormac, clip by mound extent and then create cutouts.
KAZE <- brick("E:/TRAP Workstation/Shared GIS/Satellite imagery/IKONOS/Kazanlak/ERDAS/Cormac/Kaz_E_fuse.tif")
KAZW <- brick("E:/TRAP Workstation/Shared GIS/Satellite imagery/IKONOS/Kazanlak/ERDAS/Cormac/Kaz_W_fuse.tif")

# Crop to mound extent
KAZEcropped <- crop(KAZE, st_as_sf(extent)) # crop to mound extent
KAZWcropped <- crop(KAZW, st_as_sf(extent))

### Mosaic Option 1 with mosaic() - DOES NOT WORK, USE MERGE BELOW

# Mosaicing cropped files with mosaic(r1,r2,fun = mean). Smaller files > faster performance?
# KAZcropped <- mosaic(KAZEcropped,KAZWcropped, fun = mean) # ERROR DUE TO ORIGIN PROBLEM
# origin(KAZEcropped)
# rasterOptions(tolerance = 1)

### Mosaic Option 2 with mosaic_rasters() - DOES NOT WORK, USE MERGE BELOW

# Mosaicing uncropped files with gdalutils and predefined larger extent following Matthew Bayly (2016)
# https://stackoverflow.com/questions/15876591/merging-multiple-rasters-in-r  or 
# Teng-Keng (2018) https://tengkengvang.com/2018/11/12/mosaic-or-merge-rasters-in-r/
# We can crop to mound extent later

# #first create the extent, specify projection and create empty raster for the final product
# template_kaz <- raster(extentKaz)
# template <- raster(st_as_sf(extent)) # may need to change extent for the larger files and then crop down
# proj4string(template_kaz) <- CRS('+proj=utm +zone=35 +datum=WGS84 +units=m +no_defs')
# writeRaster(template_kaz, file="KAZoriginal.tif", format="GTiff")

# # allocate files (trying with different files to see if origin problem can be overcome)
# files <- c("Kaz_E_fuse.tif",
#            "Kaz_W_fuse.tif") # maybe these are compromised??
# files <- c("mul_Kaz_East_Clip.tif",
#            "mul_Kaz_West_Clip.tif") # same problem appears and origin is consistent with fused above

# # mosaic with gdal tools for faster outcome
# mosaic_rasters(gdalfile=files,dst_dataset="KAZoriginal.tif",of="GTiff") # errors out!
# gdalinfo("KAZoriginal.tif")
# THIS APPROACH ERRORS OUT FOR THE PROVIDED FILES DUE TO PROBLEMS WITH ORIGIN 



### Mosaic Option 3 with merge()  - WORKS but histograms are misaligned. 

KAZcropped <- merge(KAZEcropped,KAZWcropped,tolerance=1) # tolerance gets around origin issue?
plotRGB(KAZcropped, stretch = "hist") # NAs are handled ok as long as E comes first..

# Writing the cropped-to-mound rasters to E:SharedGIS....Cormac on silver Seagate
# writeRaster(KAZEcropped, file="KazEcropped.tif", format="GTiff")
# writeRaster(KAZWcropped, file="KazWcropped.tif", format="GTiff")
# writeRaster(KAZcropped, file="Kazcropped.tif", format="GTiff")

