# =============================================================================
# make_zone_boxes.R - defaults/download/fire-zone-boxes.csv: each pyrome's bounding box, for the Data Sources page
# =============================================================================
# The page says whether a region folder covers a zone, as the stages' study_box does, and cannot read a shapefile. Boxes in EPSG:5070; ID = Map Code
# (1000 + PYROME), as the Fire Zones list. Usage: Rscript tools/make_zone_boxes.R <Pyromes_CONUS_20200206.shp> (from the package root)
suppressPackageStartupMessages({ library(sf); library(data.table) })
a <- commandArgs(TRUE); z <- st_transform(st_read(a[1], quiet = TRUE), 5070)
b <- rbindlist(lapply(seq_len(nrow(z)), function(i) { x <- as.numeric(st_bbox(z[i, ])); data.table(ID = 1000L + as.integer(z$PYROME[i]), Xmin = round(x[1]), Ymin = round(x[2]), Xmax = round(x[3]), Ymax = round(x[4])) }))
b <- b[, .(Xmin = min(Xmin), Ymin = min(Ymin), Xmax = max(Xmax), Ymax = max(Ymax)), by = ID][order(ID)]
fwrite(b, file.path("defaults", "download", "fire-zone-boxes.csv")); cat(nrow(b), "zones\n"); print(b[ID == 1026])
