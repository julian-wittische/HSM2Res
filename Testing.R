
# --- THRESHOLD (optional) ---------------------------------------------------
# choose threshold: e.g. maximum sensitivity+specificity using presence + background as pseudo-abs
pred_at_pres <- as.numeric(terra::extract(pred_raster, pts_thinned)[,1])
pred_at_bg   <- as.numeric(terra::extract(pred_raster, bg_sf)[,1])
# compute threshold function
library(pROC)
labels <- c(rep(1,length(pred_at_pres)), rep(0,length(pred_at_bg)))
scores <- c(pred_at_pres, pred_at_bg)
rocobj <- roc(labels, scores, quiet=TRUE)
th_opt <- coords(rocobj, "best", ret="threshold", best.method="youden")

suit_bin <- pred_raster >= th_opt
writeRaster(suit_bin, filename=file.path(out_dir,"suitability_binary.tif"), overwrite=TRUE)



# --- EXPORT FOR OMNISCAPE / CIRCUITSCAPE ------------------------------------
# Omniscape/Circuitscape accept ASCII grid / GeoTIFF of resistance (cost) or conductance (inverse).
# We wrote resistance.tif above; if you want conductance (suitable for circuitscape) write that too:
conductance <- pred_raster # suitability ~ conductance
writeRaster(conductance, filename=file.path(out_dir,"conductance.tif"), overwrite=TRUE)

# Save session info for reproducibility
writeLines(capture.output(sessionInfo()), file.path(out_dir,"sessionInfo.txt"))

cat("DONE: outputs in", out_dir, "\n")
cat("Files: suitability.tif, suitability_binary.tif, resistance.tif, conductance.tif, accum_cost_from_sources.tif\n")