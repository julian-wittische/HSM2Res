######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Fall 2025
# Data: MNHNL
# Script objective : Load mdata data and environmental data and basic homogenization

############ Preparation ----
###### Get  paths for data
source("config.R")

###### Libraries
library(ggplot2) # Plotting
library(sf) # Spatial vector data
library(terra) # Spatial raster data
library(geodata) # Download BIOCLIM data

###############################################################################.
############ Loading and checking mdata data ----
###### Raw
dry_raw <- read.csv(paste0(DATAPATH, "dryland.csv"))

### Checkpoint
dim(dry_raw)
str(dry_raw)
View(dry_raw)

### Are we happy with this species list?
sort(table(dry_raw$preferred), decreasing=TRUE)

### Date check
table(as.Date(dry_raw$date_end))

###### Keep essentials
dry_e <- dry_raw[,c("preferred", "date_end", "Lat", "Long")]

###### Make sf spatial points object
dry_sf <- st_as_sf(dry_e, coords=c("Long", "Lat"), crs="EPSG:4326")
dry_sf <- st_transform(dry_sf, crs="EPSG:2169")
### Checkpoint
ggplot() + geom_sf(data=dry_sf)

############ Environmental data ----

###### BIOCLIM
### bioclim variables for Luxembourg downloaded at 30 arc second with geodata
bio_lux <- rast(paste0(ENVIPATH,"LUX_wc2.1_30s_bio.tif"))
print(bio_lux)
plot(bio_lux)
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# BIO4 = Temperature Seasonality (standard deviation ×100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter

### Reproject
bio_lux <- project(bio_lux, "EPSG:2169")

### Crop and mask observations to only include ones from Luxembourg
lux_borders <- readRDS(paste0(ENVIPATH,"lux_borders.RDS"))
lux_borders_2169 <- st_transform(lux_borders, crs="EPSG:2169")
bio_lux_c <- crop(bio_lux, lux_borders_2169, snap="out")
bio_lux_m <- mask(bio_lux_c, lux_borders_2169)
plot(bio_lux_m$wc2.1_30s_bio_1)

###### Elevation variables
# Raw - high resolution
#DEM_50cm <- rast(paste0(ENVIPATH,"RastersLuxHighestResolution/LUX_DEM_50cm.grd"))

# Aspect
asp <-

# Slope
slo <-
  
# Roughness (classic)
rou <-
  
###### Homogenize resolution
DEM_wcres <- resample(DEM_50cm, bio_lux_m, method="average")

###### TEMP quick DEM
dem <- elevation_30s("Luxembourg", path=ENVIPATH, mask=TRUE)
dem <- project(dem, "EPSG:2169")
plot(dem)
dem <- resample(dem, bio_lux_m, method="average")

###### Grassland
GRA <- rast(paste0(ENVIPATH,"RastersLuxHighestResolution/GRA_10m.grd"))
GRA <- rast("W:/01_Services/SCR_Informations Patrimoine Naturel//_ENV_DATA_LUX/RastersLuxHighestResolution/GRA_10m.grd")



############ Essential stack ---- 
rs_e <- rast(list(min_coldest = bio_lux_m$wc2.1_30s_bio_6,
                  max_warmest = bio_lux_m$wc2.1_30s_bio_5,
                  mean_annual_T = bio_lux_m$wc2.1_30s_bio_1,
                  mean_annual_P = bio_lux_m$wc2.1_30s_bio_12,
                  T_seasonality = bio_lux_m$wc2.1_30s_bio_4,
                  dem = dem))

