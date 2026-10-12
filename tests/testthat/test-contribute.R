test_that("standardize_points derives coordinates, depth and defaults", {
  rec <- standardize_points(make_points(), full_answers, std)
  expect_equal(nrow(rec), 3)
  expect_equal(names(rec), std_names(std, "peat_data"))
  expect_true(all(rec$verbatimSRS == "EPSG:25833"))
  expect_true(all(rec$geodeticDatum == "EPSG:4326"))
  expect_equal(rec$surfacePeatDepth, c("120", "0", "45.5"))   # zero depth kept
  expect_equal(rec$easting[1], "300000")
  expect_true(all(rec$probeReachedBottom == "true"))
})

test_that("coordinates round-trip between 25833 and 4326", {
  rec <- standardize_points(make_points(), full_answers, std)
  back <- sf::st_as_sf(data.frame(lon = as.numeric(rec$longitude), lat = as.numeric(rec$latitude)),
                       coords = c("lon", "lat"), crs = 4326)
  xy <- sf::st_coordinates(sf::st_transform(back, 25833))
  expect_equal(unname(xy[, "X"]), as.numeric(rec$easting), tolerance = 0.5)
  expect_equal(unname(xy[, "Y"]), as.numeric(rec$northing), tolerance = 0.5)
})

test_that("the three bottom-reached answers map as planned", {
  p <- make_points()
  bottom <- function(b) standardize_points(p, modifyList(full_answers, list(bottom = b)), std)$probeReachedBottom
  expect_true(all(bottom("all") == "true"))
  expect_true(all(bottom("some") == "true"))
  expect_true(all(bottom("none") == "false"))
})

test_that("uploaded probeReachedBottom wins and yes/no spellings are converted", {
  p <- make_points(data.frame(probeReachedBottom = c("ja", "Nei", "TRUE")))
  rec <- standardize_points(p, modifyList(full_answers, list(bottom = "none")), std)
  expect_equal(rec$probeReachedBottom, c("true", "false", "true"))
  expect_equal(normalize_boolean(c("Yes", "NO", "1", "0", "maybe", NA)),
               c("true", "false", "true", "false", NA, NA))
  # empty upload values mean "not sure", which is false
  p <- make_points(data.frame(probeReachedBottom = c("yes", "", "no")))
  expect_equal(standardize_points(p, full_answers, std)$probeReachedBottom, c("true", "false", "false"))
})

test_that("extra upload columns are kept after the standard columns", {
  p <- make_points(data.frame(vegetation = c("a", "b", "c")))
  rec <- standardize_points(p, full_answers, std)
  expect_equal(names(rec), c(std_names(std, "peat_data"), "vegetation"))
})

test_that("per-row edits are applied only where given", {
  rec <- standardize_points(make_points(), full_answers, std)
  ed <- empty_edits(3)
  ed$probeReachedBottom[2] <- "false"
  out <- apply_edits(rec, ed)
  expect_equal(out$probeReachedBottom, c("true", "false", "true"))
  expect_identical(apply_edits(rec, empty_edits(2)), rec)   # wrong size is ignored
})

test_that("other depth resolutions are written and labelled", {
  expect_equal(resolution_value(2.5), "Nearest 2.5cm")
  expect_equal(resolution_value(1), "Nearest 1cm")
  expect_equal(resolution_label("Nearest 2.5cm", std, "no"), "Nærmeste 2,5 cm")
  expect_equal(resolution_label("Nearest 2.5cm", std, "en"), "Nearest 2.5 cm")
})

test_that("metadata gets bounding box and temporal coverage from the records", {
  p <- make_points()
  rec <- standardize_points(p, modifyList(full_answers, list(recordDate = "2024-06-01/2024-06-03")), std)
  m <- build_metadata(full_meta, rec, std)
  expect_equal(m$temporalCoverageStartDate, "2024-06-01")
  expect_equal(m$temporalCoverageEndDate, "2024-06-03")
  expect_lt(as.numeric(m$westBoundingCoordinate), as.numeric(m$eastBoundingCoordinate))
  expect_lt(as.numeric(m$southBoundingCoordinate), as.numeric(m$northBoundingCoordinate))
  expect_equal(names(m), std_names(std, "metadata"))
})

test_that("the written package is RFC 4180 CSV with UTF-8 and CRLF", {
  p <- make_points()
  ans <- modifyList(full_answers, list(surveyor = 'Åse "Ø" Nordmann, æøå', peatDepthRemarks = "line"))
  rec <- standardize_points(p, ans, std)
  meta <- build_metadata(modifyList(full_meta, list(abstract = "Komma, og \"sitat\"")), rec, std)
  dir <- file.path(tempdir(), "pkg_test")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- write_standard_package(rec, meta, std, dir)
  expect_true(all(file.exists(paths)))

  raw <- readBin(paths[1], "raw", file.size(paths[1]))
  expect_true(any(raw == as.raw(0x0d)))   # CRLF
  back <- read.csv(paths[1], colClasses = "character", encoding = "UTF-8", na.strings = "")
  expect_equal(nrow(back), 3)
  expect_equal(names(back), names(rec))
  expect_equal(back$surveyor[1], 'Åse "Ø" Nordmann, æøå')
  expect_true(all(back$verbatimSRS == "EPSG:25833"))

  mback <- read.csv(paths[2], colClasses = "character", encoding = "UTF-8", na.strings = "")
  expect_equal(nrow(mback), 1)
  expect_equal(mback$abstract, "Komma, og \"sitat\"")

  readme <- paste(readLines(paths[3], warn = FALSE), collapse = "\n")
  expect_match(readme, "TDWG Boolean")
  expect_match(readme, "EPSG:25833")
})

test_that("licence choices stay valid under the NE standard", {
  keys <- std_choice_keys(std$no_profile$choices$license)
  nlod <- std_choice(std, "license", grep("NLOD", keys, value = TRUE)[1])
  expect_false(is.null(nlod))
  expect_equal(nlod$value, "Other")
  expect_equal(nlod$rights, "NLOD 2.0")
  offered <- vapply(std$no_profile$choices$license, `[[`, "", "value")
  expect_false(any(c("OGL", "NCGL") %in% offered))
})

test_that("translation lookup falls back to the key", {
  expect_equal(contribute_tr("Bidra med data", "no"), "Bidra med data")
  expect_equal(contribute_tr("Bidra med data", "en"), "Contribute data")
  expect_equal(contribute_tr("no such string", "en"), "no such string")
})

test_that("the methods draft leaves out an 'other' probe and uses the right article", {
  recs <- standardize_points(make_points(), full_answers, std)
  m <- function(probe) draft_methods(modifyList(full_answers, list(probeType = probe)), recs, std, "en")
  expect_false(grepl("using", m("other")))
  expect_match(m("peat probe"), "using a peat probe")
  expect_match(m("avalanche probe"), "using an avalanche probe")
})

test_that("store_submission is a Phase 2 stub", {
  expect_warning(res <- store_submission(character(), list()), "not implemented")
  expect_false(res)
})
