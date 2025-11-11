######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Fall 2025
# Data: MNHNL
# Script objective : Preparing data for habitat suitability modelling

#######
library(sf)
library(terra)
library(dplyr)
library(ENMeval)
library(maxnet)
#library(blockCV)
library(spThin)
library(gdistance)

out_dir  <- "hsm_out"
dir.create(out_dir, showWarnings=FALSE)



###### Preparing points
# Remove exact duplicates
dry_sf <- dry_sf %>% distinct(geometry, .keep_all = TRUE)

pts_df <- st_coordinates(dry_sf) |> as.data.frame()
pts_df$species <- "sp"

thin_out <- thin(
  loc.data = pts_df,
  lat.col = "Y",
  long.col = "X",
  spec.col = "species",
  thin.par = 1000,
  reps = 1,write.files = TRUE,              # must write temporarily to capture
  locs.thinned.list.return = TRUE,
  out.dir=out_dir
)

thinned_df <- thin_out[[1]]
pts_thinned <- st_as_sf(thinned_df, coords = c("Longitude","Latitude"), crs = 2169)

plot(pts_thinned)

pts_rast <- rasterize(vect(dry_sf), rs_e, field=1, fun='sum', background=0)

# smooth kernel density (focal)
w <- matrix(1, nrow=5, ncol=5)
bias_rast <- focal(pts_rast, w=w, fun=sum, na.rm=TRUE)

# normalize to probabilities
bias_vals <- as.matrix(bias_rast, wide=TRUE)
bias_vals[is.na(bias_vals)] <- 0
bias_vals <- bias_vals / sum(bias_vals, na.rm=TRUE)
bias_rast[] <- bias_vals

writeRaster(bias_rast, filename=file.path(out_dir,"bias_raster.tif"), overwrite=TRUE)

#SAMPLE BACKGROUND POINTS (target-group / bias sampling) 
n_bg <- 20000  # number of background points
set.seed(42)
# sample with probability proportional to bias raster
bg_cells <- sample(1:ncell(bias_rast), size = n_bg, prob = values(bias_rast), replace = TRUE)
bg_coords <- xyFromCell(bias_rast, bg_cells)
bg_sf <- st_as_sf(data.frame(bg_coords), coords = c("x","y"), crs = 2169)

pres_vals <- extract(rs_e, pts_thinned)
bg_vals   <- extract(rs_e, bg_sf)

# remove rows with NA (outside env extent)
ok_pres <- complete.cases(pres_vals)
pres_vals <- pres_vals[ok_pres, , drop=FALSE]
pts_thinned <- pts_thinned[ok_pres, ]

ok_bg <- complete.cases(bg_vals)
bg_vals <- bg_vals[ok_bg, , drop=FALSE]
bg_sf <- bg_sf[ok_bg, ]

# format for maxnet/ENMeval: presence background dataframes
pres_df <- data.frame(pres_vals)
bg_df   <- data.frame(bg_vals)

# SPATIAL BLOCK CROSS-VALIDATION (blockCV)
# create spatial blocks on the full set of presence+background for spatial CV
all_pts_sf <- rbind(
  pts_thinned %>% mutate(pb=1),
  bg_sf %>% mutate(pb=0)
)

# blockCV needs SpatialPointsDataFrame or sf; use sf no i changed to cv_spatial
# 
# set.seed(123)
# sb <- spatialBlock(speciesData = all_pts_sf,
#                    theRange = 50000,         # block size (meters) - choose according to species dispersal; adjust!
#                    k = 5,
#                    selection = "random",
#                    showBlocks = FALSE)

# # get fold assignments for presences only
# pb_folds <- sb$foldID[1:nrow(pts_thinned)]  # first rows correspond to presences
#
