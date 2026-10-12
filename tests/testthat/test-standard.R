test_that("the standard and overlay load and are consistent", {
  expect_true(length(std_names(std, "peat_data")) > 0)
  expect_true("probeReachedBottom" %in% std_names(std, "peat_data"))
  expect_false("gridReference" %in% std_names(std, "peat_data"))   # hidden by the profile
  expect_true("gridReference" %in% std_names(std, "peat_data", include_hidden = TRUE))
})

test_that("attribute order matches peat_data_attributes.csv", {
  csv <- read.csv(file.path(root, "standard", "peat_data_attributes.csv"), stringsAsFactors = FALSE)
  nm_col <- intersect(c("name", "attribute"), names(csv))[1]
  expect_false(is.na(nm_col))
  expected <- csv[[nm_col]][order(csv$order)]
  expect_equal(std_names(std, "peat_data", include_hidden = TRUE), expected)
})

test_that("a complete record set has no errors", {
  rec <- standardize_points(make_points(), full_answers, std)
  iss <- validate_against_standard(rec, std, "peat_data")
  expect_equal(sum(iss$level == "error"), 0, info = paste(iss$message, collapse = "; "))
})

test_that("missing probeReachedBottom is reported", {
  rec <- standardize_points(make_points(), full_answers, std)
  rec$probeReachedBottom <- NULL
  iss <- validate_against_standard(rec, std)
  expect_true(any(iss$field == "probeReachedBottom" & iss$level == "error"))
  rec <- standardize_points(make_points(), full_answers, std)
  rec$probeReachedBottom[2] <- NA
  iss <- validate_against_standard(rec, std)
  expect_equal(iss$row[iss$field == "probeReachedBottom"], 2L)
})

test_that("lat/lon without geodeticDatum is an error", {
  rec <- standardize_points(make_points(), full_answers, std)
  rec$geodeticDatum <- NA
  iss <- validate_against_standard(rec, std)
  expect_true(any(iss$rule == "datum_with_latlon" & iss$level == "error"))
})

test_that("elevation without verticalDatum is an error", {
  rec <- standardize_points(make_points(), full_answers, std)
  rec$surfaceElevation <- "100"
  rec$verticalDatum <- NA
  iss <- validate_against_standard(rec, std)
  expect_true(any(iss$rule == "vertical_datum_with_elevation" & iss$level == "error"))
  rec$verticalDatum <- "EPSG:5941"
  iss <- validate_against_standard(rec, std)
  expect_false(any(iss$rule == "vertical_datum_with_elevation"))
})

test_that("bad enum, number, boolean and date values are caught", {
  rec <- standardize_points(make_points(), full_answers, std)
  rec$probeType[1] <- "banana"
  rec$surfacePeatDepth[2] <- "deep"
  rec$probeReachedBottom[3] <- "Yes"      # must be lowercase true/false since C7
  rec$recordDate[1] <- "01.06.2024"
  iss <- validate_against_standard(rec, std)
  expect_true(any(iss$field == "probeType" & iss$rule == "type_enum"))
  expect_true(any(iss$field == "surfacePeatDepth" & iss$rule == "type_number"))
  expect_true(any(iss$field == "probeReachedBottom" & iss$rule == "type_boolean"))
  expect_true(any(iss$field == "recordDate" & iss$rule == "type_date"))
})

test_that("a missing location is an error, and depth or presence is needed", {
  rec <- standardize_points(make_points(), full_answers, std)
  rec[c("latitude", "longitude", "easting", "northing")] <- NA
  iss <- validate_against_standard(rec, std)
  expect_true(any(iss$rule == "location_one_of"))
  rec <- standardize_points(make_points(), full_answers, std)
  rec$surfacePeatDepth <- NA
  iss <- validate_against_standard(rec, std)
  expect_true(any(iss$rule == "depth_or_presence"))
})

test_that("ISO dates and ranges are recognised", {
  expect_equal(is_iso_date(c("2024-06-01", "2024-06-01/2024-06-03", "2024-13-40", "June")),
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that("messages come in both languages and summarise_issues collapses rows", {
  rec <- standardize_points(make_points(), modifyList(full_answers, list(surveyor = "")), std)
  no <- validate_against_standard(rec, std, lang = "no")
  en <- validate_against_standard(rec, std, lang = "en")
  expect_false(identical(no$message, en$message))
  s <- summarise_issues(en)
  expect_equal(s$n[s$field == "surveyor"], 3)
})

test_that("required metadata terms are enforced", {
  rec <- standardize_points(make_points(), full_answers, std)
  ok <- build_metadata(full_meta, rec, std)
  expect_equal(sum(validate_against_standard(ok, std, "metadata")$level == "error"), 0)
  bad <- build_metadata(full_meta[setdiff(names(full_meta), "license")], rec, std)
  iss <- validate_against_standard(bad, std, "metadata")
  expect_true(any(iss$field == "license" & iss$level == "error"))
})
