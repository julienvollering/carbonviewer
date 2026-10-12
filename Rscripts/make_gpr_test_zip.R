# Convert the Orskogfjellet GPR peat depth dataset into a CarbonViewer upload zip
# (peat_depth_samples.csv with X, Y, peat_depth_cm + a bounding-box shapefile).
#
# Source: https://doi.org/10.6073/pasta/6ce440152f693f2156bf5b692a2e7917
#
# Usage (from the repository root):
#   Rscript Rscripts/make_gpr_test_zip.R test/OrskogfjelletGPR.csv test/OrskogfjelletGPR.zip

library(sf)

args <- commandArgs(trailingOnly = TRUE)
in_csv <- if (length(args) >= 1) args[[1]] else "test/OrskogfjelletGPR.csv"
out_zip <- if (length(args) >= 2) args[[2]] else "test/OrskogfjelletGPR.zip"

# Coordinates are written in ETRS89 / UTM 33N, the system CarbonViewer
# calculates in, so no reprojection is needed on upload.
target_crs <- 25833

raw <- read.csv(in_csv, encoding = "UTF-8")
raw <- raw[!is.na(raw$longitude) & !is.na(raw$latitude) &
             !is.na(raw$surfacePeatDepth), ]

# Source coordinates are WGS84 (geodeticDatum https://epsg.io/4326); depth is in cm.
pts <- st_transform(
  st_as_sf(raw, coords = c("longitude", "latitude"), crs = 4326),
  target_crs
)
xy <- st_coordinates(pts)

samples <- data.frame(
  X = xy[, 1],
  Y = xy[, 2],
  peat_depth_cm = raw$surfacePeatDepth
)

# Simple bounding box of all measurements as the study area.
study_area <- st_as_sfc(st_bbox(pts))
study_area <- st_sf(id = 1L, geometry = study_area)

# Write everything into a clean temporary directory, then zip flat.
tmp <- tempfile("gprzip_")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

write.csv(samples, file.path(tmp, "peat_depth_samples.csv"), row.names = FALSE)
st_write(study_area, file.path(tmp, "study_area.shp"), quiet = TRUE)

files <- c("peat_depth_samples.csv", paste0("study_area.", c("shp", "shx", "dbf", "prj")))
stopifnot(all(file.exists(file.path(tmp, files))))

out_zip <- normalizePath(out_zip, mustWork = FALSE)
if (file.exists(out_zip)) file.remove(out_zip)
old <- setwd(tmp)
on.exit(setwd(old), add = TRUE)
utils::zip(out_zip, files, flags = "-q")

message("Wrote ", out_zip, " with ", nrow(samples), " measurements")
