############################################################################################################
#             Creating a clean 2010 verified mound dataset
############################################################################################################

# Goal
# Create an analysis-ready dataset of all features visited in Yambol in 2010.

# Requires couple prerequisites, such as 
# 0) loading the output of joined/merged 2010 bara and adela datasets 
# 1) dropping and renaming columns to achieve consistency between datasets,  
# 2) aggregating notes into fewer columns
# 3) eliminating columns irrelevant at present (e.g. spatial join results that are not shared among datasets) 
# 4) cleaning up datatypes, and checking the consistency of classifications (Type, Height, Dimensions)

# Library
library(tidyverse)
library(lubridate)

# Load the inputs
df_name <- c("mnd2010")
if (exists(df_name)){
  is.data.frame(get(df_name))
}  else source("scripts/02_2010dataTRAP.R")

### 2010 Dataset

# Input is mnd2010 (n = 406), a conservative result of a left join between adela and bara 2010 datasets (m2010mounds),
# filtered by type == mound. 
# Another potential input is m2010leftj (n = 444), which is a more liberal result of left join with 38 features that were mapped as mounds, look like mounds, but are 
# bunkers, waterstations and other features instead. 

names(mnd2010)

# Drop undesired columns. If you are running this the first time, uncomment.
# Drop unwanted columns to get to 37 columns from original 58
m2010 <- mnd2010 %>% 
  select(-one_of("Excav", "Necropolis","ElevationTopo", "Certainty", "GC",
                 "Leader", "Datum" ,"SurfaceMaterial","SampleCollected","uuid","createdBy",
                 "Latitude","Longitude","Northing","Easting","Source_1","GC_1",
                 "Mound_ID","DateCompl0","y_proj","x_proj"))

# Rename columns and consolidate notes columns (using https://stackoverflow.com/questions/50845474/concatenating-two-text-columns-in-dplyr)
names(m2010)

m2010 <- m2010 %>% 
  rename(TopoMapHeight= Note_1) %>% 
  rename(TypeBara=Type, LU_AroundRS = Landuse_AroundRS, LU_TopRS=Landuse_TopRS, 
         DiameterMax = Length.x, Diameter_Bara= Length.y, 
         PrincipalSourceOfImpact = Principal, 
         ArchaeologicalPotential=ArcheoPotential, 
         MostRecentDamageWithin=When, RSNotes = ends_with("18")) %>% 
  unite(RTDescription, c("RTNumber", "RTPosition", "RTDescription"), sep = ";", remove = TRUE, na.rm = TRUE) 

# Check which TopoID to retain  - .x is better
problem2010TopoID <- which(!m2010$TopoID.x%in%m2010$TopoID.y)  # 35 discrepancies in Topo IDs btw adela and bara
m2010$TopoID.y[problem2010TopoID] # most are zeroes in Bara, only 200244 a problem, which is perhaps a typo?? CHECK IN GE

# Fix Dates
m2010 <- m2010 %>%
  mutate(Date=paste(mnd2010$Date, sep="-","2010")) %>%
  mutate(Date=dmy(Date)) %>%
  glimpse()

# length(which(is.na(m2010$Date))) # ok 73 don't have a date

############################################################################################################
# Generate a finalized 2010 conservative dataset (n = 406, 30 columns) 
############################################################################################################

# Additional renaming and dropping of columns
# https://suzan.rbind.io/2018/01/dplyr-tutorial-1/#selecting-columns-based-on-regex

m2010 <- m2010 %>% 
  rename(TypeGE=SomethingPresentOntheGround, LU_Around = LandUseAround, LU_Top=LanduseOn, LU_Source=LandUseSource,
         Width_Bara = Width.y, Height_Bara = Height.y, Condition_Bara = Condition, Condition = CRM, 
         DiameterMin = Width.x, Height_Adela = Height.x, RT_numberGE = RT_number, RT_number = RTDescription, 
         TopoID2017 = `2017identifier`, TopoID = TopoID.x,
         HeightMap = TopoMapHeight, Source = Source.y, 
         DiameterMax_Bara=Diameter_Bara, DiameterMin_Bara=Width.y) %>% 
  unite(Name_BG, c("BLG_Name","NameTopo"), sep = ";", remove = TRUE, na.rm = TRUE) %>% 
  unite(AllNotes, c("Notes", "Description"), sep = "; Bara:", remove = TRUE, na.rm = TRUE) %>% 
  unite(RT_Number, c("RT_number", "RT_numberGE"), sep = "; GE:", remove = TRUE, na.rm = TRUE) %>% 
  select(-one_of("Source.x", "TopoID.y"))   #remove needless


############################################################################################################
# Streamline the 2010 master dataset
############################################################################################################
#
# Streamline Height between Bara and Adela as 12 values do not agree

m2010$Height_Adela[!m2010$Height_Adela%in%m2010$Height_Bara]
heightissue <- !m2010$Height_Adela%in%m2010$Height_Bara


# Height differences owe to: 5 to atlas values in Adela, 7 to NAs in Bara
m2010 %>% 
  select(TRAP, Height_Adela, Height_Bara) %>% 
  filter(heightissue)

# reviewing the images, Height_Bara corresponds better to photograph in 9038-9057. 
# Exception is 9159,9160, where Adela's estimate is better.  9307 - 9438 are unverifim2010le due to photo loss
# Where Bara is NA, then Adela should stay NA as well in this subset.

which(m2010$TRAP == 9038)

# overwrite Heights
m2010$Height_Adela[25] <- "2.0" #overwriting 9038 height with Bara value 
m2010$Height_Adela[34] <- "1.5"  #overwriting 9050 height with Baras value
m2010$Height_Adela[38] <- "0.5"  #overwriting 9057 height with Baras value
m2010$TRAP[38] 
m2010[278,]

# Which Height column is better? There are more missing values in Height_Bara
which(is.na(as.numeric(m2010$Height_Adela)))  # 6 NAs get introduced by coercion to numeric
which(is.na(as.numeric(m2010$Height_Bara)))  # 48 NAs get introduced to Bara

# Height_Adela can now be considered Height_Max
m2010 <- m2010 %>% 
  rename(HeightMax=Height_Adela) 

# Streamline Type attribute in 2010

levels(as.factor(m2010$TypeGE))
levels(as.factor(m2010$TypeBara))
m2010 %>% filter(TypeBara == "Surface Scatter")

# Review type in TypeBara as Type is only 'mound'; need to fix 45 NAs if we are to use this column
m2010 %>% 
  select(TRAP, TypeGE, TypeBara, HeightMax, Condition) %>% 
  group_by(TypeBara) %>% 
  summarize(Havg = mean(!is.na(HeightMax)), n()) 

# If we exclude Burial Mound types, the NAs don't show
m2010 %>% 
  select(TRAP, TypeGE, TypeBara, HeightMax, Condition) %>% 
  filter(TypeBara != "Burial Mound") %>% 
  group_by(TypeBara) %>%
  tally()

# Eliminate NAs in Type: 

# first, create a a temp df that contains all NAs from TypeBara and verify them in photos
temp <- m2010 %>% 
  select(TRAP, TypeGE, TypeBara, HeightMax, DiameterMax, DiameterMin, Condition, AllNotes) %>% 
  filter(is.na(TypeBara)) # %>% 

#  filter(HeightMax<1.1 | DiameterMax <15 | DiameterMin < 15) # couple Heights are wrong .e.g. 9302 is clearly 1m at least
#  Photos show 9107 and 9106 are extinct, 9097 is excavated? 9098 is not a mound?, 9089 is a mound


# Convert Photo consultation into classification vectors:  
mounds <- c(9089, 9090, 9091, 9092, 9093, 9094, 9095, 9096, 9099:9105, 9108, 9109, 9305:9312)  
ext <- c(9097, 9106,9107,9214, 9303, 9304, 9301, 9313, 9435, 9438)  
unc <-  c(9077, 9098)
`%nin%` = Negate(`%in%`)
remnants <-  temp$TRAP[temp$TRAP%nin%c(mounds,ext,unc)] 


# Replace NA values in Type using Classification vectors via case_when
m2010 <- m2010 %>% 
  #select(TRAP, TypeBara) %>% 
  #filter(is.na(TypeBara)) %>% 
  mutate(TypeBara = case_when(TRAP%in%mounds ~  "Burial Mound", 
                              TRAP%in%ext ~ "Extinct Burial Mound",
                              TRAP%in%unc ~ "Burial Mound?", 
                              TRAP%in%remnants ~ "Burial Mound",
                              TRAP%nin%c(mounds,ext,unc)~ TypeBara)) %>% 
  rename(Type = TypeBara)


# Check completeness
levels(as.factor(m2010$Type)) # looks ok, lets' see a tally

m2010 %>% 
  group_by(Type) %>% 
  tally()

m2010 %>% 
  select(TRAP, TypeGE, Type, HeightMax, Condition) %>% 
  group_by(Type) %>% 
  summarize(Havg = mean(!is.na(HeightMax)), n())


# Remove temps
rm(mounds, ext, unc, remnants, temp, heightissue)
rm(problem2010TopoID)

# Create clean 2010
head(m2010,2)



# Further Type classification vs Dimension checks - this applies to the entire master dataset,
# but is worth investigating at campaign-level as well to detect inter-annual shift in classification
#
# mndXXXX %>% 
#   select(TRAP, Source, DiameterMax, DiameterMin, HeightMax, Condition, Notes, Type) %>% 
#   filter(HeightMax<0.6 | DiameterMax <15 | DiameterMin < 15) %>% 
#   tail(13)
# mutate(Type = "Extinct Burial Mound") %>% 
#   filter(HeightMax == 0) %>% 
#   mutate(Type = "Uncertain Mound")
# 
# levels(as.factor(mnd2017$Type))

