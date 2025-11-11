# --- SIMPLE CONNECTIVITY: gdistance least-cost corridors --------------------
# Build transition from resistance (cost) raster
r_rast <- raster::raster(file.path(out_dir,"resistance.tif"))
tr <- gdistance::transition(1 / r_rast, transitionFunction = mean, directions = 8)
tr <- geoCorrection(tr, type="c")

# choose focal points to connect (e.g. presences or user-defined cores)
cores <- as.data.frame(st_coordinates(pts_thinned))   # use thinned presences
# compute cumulative cost-distance from each core (expensive) - demonstrate for first N cores
Ncores <- min(50, nrow(cores))   # limit to 50 to keep computation feasible
cost_stack <- list()
for(i in seq_len(Ncores)){
  from <- as.matrix(cores[i,])
  cd <- costDistance(tr, SpatialPoints(from, proj4string=CRS(proj4string(r_rast))))
  # costDistance returns matrix; we project as needed; more complete code would compute raster of cumulative cost
}
# Note: full least cost corridor mapping can be done with functions such as accCost in gdistance
# Example: cumulative cost from a set of source points:
sources <- SpatialPoints(cores[1:Ncores,], proj4string = CRS(proj4string(r_rast)))
accum <- accCost(tr, sources)
writeRaster(accum, filename=file.path(out_dir,"accum_cost_from_sources.tif"), overwrite=TRUE)