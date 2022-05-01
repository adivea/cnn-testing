# Archeological Burial Mounds CNN Classifier

The goal of this project was to build a machine learning system that could identify potential burial mounds in satellite images to a high degree of accuracy. The IKONOS satellite images used come from the GeoEye Foundation and cover the Upper Tundzha watershed in the Kazanlak Valley, Bulgaria. Burial mounds dating back 2000+ thousand years are found across the valley, with the largest and richest giving the valley the nickname "Valley of the Thracian Kings".

To accomplish this goal, the project builds a CNN classifier model which takes in cropped tiles of a satellite image and predicts whether the area within is likely to contain a mound or not. The image's projected coordinates can then be used to determine the exact location of the mound. While the project focused on this one region, the greater interest lay in whether machine learning tools could be used to assist archeological teams in their field work, by identifying areas of interest before teams have to be sent out or, furthermore, to detect change to the surface mounds and thus help direct resources to where they were most needed

## The Repository

This repository serves for 

1. training data generation and 
2. validation purposes

## 1. Training data generation

To generate training data for CNN classifier, we mosaic the two scenes of IKONOS satellite imagery and then cut out stamps of 150x side centered over the 773 points that mark documented burial mounds. In 2022 rerun of the classifier the 773 cutouts are further filtered for those where a mound is clearly visible in the centre of the image. These two scripts help with te training data generation

- MOSAIC.R - pipeline to mosaic the two scenes of the IKONOS image into one merged and cropped image
- STAMPS.R - workflow to generate polygons of 150x150m for satellite image cutouts. Locations of burial mounds are used as centroids for polygon generation 

## 2. Validation 

We load the classification csv with predictions of a 'mound' or 'not a mound' for each 150x150px tile in the satellite image. We select the records where mound-probability is 60% and higher and then create a grid of polygons grown out of each qualifying coordinate as from origin. The resulting polygons mark spatial boundaries of tiles classified as containing a mound. These polygons are intersected with the archaeological record of burial mound, in the shape of points. Points that intersect the polygons are true positives, or detected mounds. Points outside the polygons are missed, false negative, mounds.

The workflow is broken up into two scripts, data loading and data validation:

- CNN01_LoadData2021.R and CNN01_LoadData2022.R where prediction data are loaded and polygon grids constructed
- CNN02_Validate2021.Rmd and CNN02_Validate2022.Rmd where the prediction data are intersected with mound data and evaluated for success

Validation is performed twice in the repository: 

- First time for 2021 CNN predictions trained on a cumulative collection of training data based on all 773 field points (2021-10-25_predictions). In this dataset the mound-probability comes in eponymous columns with values from 0-1
- Second time for 2022 CNN predictions trained on a selection of training data (n=249), filtered for mounds actually visible in the satellite image(2022-03-10_predictions). In this dataset, mound-probability values are coded with MOUND being 0 and NOT MOUND being 1, and thus require inversion. 

The mechanics of validation follow the workflow the two iterations: For 2021, the validation grid is build from two sets of predictions, while in 2022, only a single grid is created and probability values are inverted to correspond to mound-probability. 

## License
[CC-BY-SA 4.0]

## DOI
[Here will be DOI or some other identifier once we have it]

### References
[Here will go related articles or other sources we will publish/create]

---
# How to use this repository

## Sources and prerequisites
To run the validation, you need to have R software and a couple of libraries, such as sf, raster, mapview or leaflet, and the tidyverse. They are listed at the start of the scripts.


## Data
Point, polygon, and prediction data are in the data folder. Spatial data were collected by the [Tundzha Regional Archaeological Project](https://tundzha.org). The Kazanlak mosaiced satellite image is not needed for the validation. It may be used for visualisation but can be substituted with interactive mapview(). 

If you would like to create your own mosaic of the IKONOS scenes, you can download the full-resolution images manually from our public archive at [www.sciencedata.dk](https://sciencedata.dk/shared/104dc1b7a08529650379d858979ae104) folder, or directly with with `file.download()` using these direct links for [West](https://sciencedata.dk/public/104dc1b7a08529650379d858979ae104/KazWestfused.tif) and [East](https://sciencedata.dk/public/104dc1b7a08529650379d858979ae104/KazEastfused.tif) respectively. Beware of 2Gb file size.

## Authors

* Adela Sobotkova [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-4541-3963), SDAM project, adela@cas.au.dk
