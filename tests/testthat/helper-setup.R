# Sources the standard and contribute helpers without starting the app.
root <- normalizePath(testthat::test_path("..", ".."))
options(carbonviewer.app_dir = root)
source(file.path(root, "appScripts", "standard.R"))
source(file.path(root, "appScripts", "contribute.R"))

std <- load_standard()

# Three points in EPSG:25833 (southern Norway), with a zero depth
make_points <- function(extra = NULL) {
  df <- data.frame(x = c(300000, 300010, 300020), y = c(6600000, 6600010, 6600020),
                   torvdybde_cm = c(120, 0, 45.5))
  if (!is.null(extra)) df <- cbind(df, extra)
  sf::st_as_sf(df, coords = c("x", "y"), crs = 25833, remove = FALSE)
}

full_answers <- list(
  recordDate = "2024-06-01", surveyor = "Test Org", organizationName = "Test Org",
  location = "Testmyra", probeType = "peat probe", peatDepthResolution = "Nearest 1cm",
  coordinateUncertainty = 5, bottom = "all")

# The required metadata terms
full_meta <- list(
  title = "Peat depths Testmyra 2024", organizationName = "Test Org",
  abstract = "Peat depth probing at Testmyra.", license = "CC-BY",
  contactElectronicMailAddress = "someone@example.org")
