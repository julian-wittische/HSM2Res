pres_coords <- st_coordinates(pts_thinned)
bg_coords_df  <- st_coordinates(bg_sf)
# Define feature/class settings grid for tuning
fc <- c("L","LQ","H","LQH")
rm_vals <- c(0.5,1,2)  # regularization multipliers to try

# ENMeval tune with spatial cross-validation
ev <- ENMevaluate(
  occs = pres_coords,
  envs = rs_e,
  bg = bg_coords_df,
  tune.args = list(
    fc = fc,
    rm = rm_vals
  ),
  partitions = "block",  # Try this instead
  algorithm = "maxnet",
  parallel = TRUE
)


# choose best model by AICc (or by AUC on test)
best_idx <- which.min(ev@results$AICc)
best_settings <- ev@results[best_idx, c("fc","rm")]

# final maxnet model using best settings
# ENMeval can output the best model; use its models list
best_model <- ev@models[[best_idx]]

# --- PREDICT TO RASTER ------------------------------------------------------
pred_raster <- predict(rs_e, best_model, type="cloglog", na.rm = TRUE)  # gives continuous suitability 0-1
# write
writeRaster(pred_raster, filename=file.path(out_dir,"suitability.tif"), overwrite=TRUE, filetype="GTiff")


# --- MAKE RESISTANCE SURFACE ------------------------------------------------
# A common transform: resistance = 1000 * (1 - suitability) + 1  (higher = harder to move)
suit_vals <- values(pred_raster)
res_vals  <- 1 + 999*(1 - suit_vals)   # range 1..1000
res_rast  <- pred_raster
values(res_rast) <- res_vals
writeRaster(res_rast, filename=file.path(out_dir,"resistance.tif"), overwrite=TRUE)

