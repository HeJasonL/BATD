context("Configuration Loading and Management")

test_that("load_config() loads site_protocols successfully", {
  skip_if_not(
    file.exists(system.file("config/site_protocols.yaml", package = "BATD")),
    "site_protocols.yaml not found"
  )

  config <- load_config("site_protocols")
  expect_is(config, "list")
  expect_true("sites" %in% names(config))
  expect_true(length(config$sites) > 0)
})

test_that("load_config() loads defaults successfully", {
  skip_if_not(
    file.exists(system.file("config/defaults.yaml", package = "BATD")),
    "defaults.yaml not found"
  )

  config <- load_config("defaults")
  expect_is(config, "list")
  expect_true(length(config) > 0)
})

test_that("load_config() returns NULL for missing file", {
  config <- load_config("nonexistent_file")
  expect_null(config)
})

test_that("load_config() handles .yaml extension correctly", {
  skip_if_not(
    file.exists(system.file("config/defaults.yaml", package = "BATD")),
    "defaults.yaml not found"
  )

  # Should work with and without extension
  config_with_ext <- load_config("defaults.yaml")
  config_without_ext <- load_config("defaults")

  expect_equal(names(config_with_ext), names(config_without_ext))
})

test_that("get_site_protocols() retrieves KKI protocols", {
  config <- load_config("site_protocols")
  skip_if(is.null(config))

  kki_info <- get_site_protocols("KKI", config)
  expect_is(kki_info, "list")
  expect_true("protocols" %in% names(kki_info))
  expect_true(length(kki_info$protocols) > 0)
})

test_that("get_site_protocols() retrieves UCLA protocols", {
  config <- load_config("site_protocols")
  skip_if(is.null(config))

  ucla_info <- get_site_protocols("UCLA", config)
  expect_is(ucla_info, "list")
  expect_true("name" %in% names(ucla_info))
  expect_match(ucla_info$name, "California")
})

test_that("get_site_protocols() returns NA site for unknown site", {
  config <- load_config("site_protocols")
  skip_if(is.null(config))

  unknown_info <- get_site_protocols("UNKNOWN_SITE", config)
  expect_is(unknown_info, "list")
  # Should fall back to NA site
  expect_true(length(unknown_info) > 0)
})

test_that("get_protocol_name() retrieves protocol 100 names", {
  config <- load_config("site_protocols")
  skip_if(is.null(config))

  # Test multiple sites
  kki_name <- get_protocol_name(100, "KKI", config)
  expect_is(kki_name, "character")
  expect_true(!is.na(kki_name))
  expect_match(kki_name, "Detection|Discrimination", ignore.case = TRUE)
})

test_that("get_protocol_name() returns NA for unknown protocol", {
  config <- load_config("site_protocols")
  skip_if(is.null(config))

  unknown <- get_protocol_name(99999, "UCLA", config)
  expect_true(is.na(unknown))
})

test_that("get_defaults() retrieves analysis settings", {
  config <- load_config("defaults")
  skip_if(is.null(config))

  analysis <- get_defaults("analysis", config)
  expect_is(analysis, "list")
  expect_true("session_gap_threshold" %in% names(analysis))
  expect_true(is.numeric(analysis$session_gap_threshold))
})

test_that("get_defaults() retrieves extraction settings", {
  config <- load_config("defaults")
  skip_if(is.null(config))

  extraction <- get_defaults("extraction", config)
  expect_is(extraction, "list")
  expect_true("min_confidence_threshold" %in% names(extraction))
})

test_that("get_defaults() retrieves entire config when section is NULL", {
  config <- load_config("defaults")
  skip_if(is.null(config))

  full_config <- get_defaults(NULL, config)
  expect_is(full_config, "list")
  expect_true(length(full_config) > 1)
})

test_that("validate_configuration() validates site_protocols", {
  config <- load_config("site_protocols")
  skip_if(is.null(config))

  is_valid <- validate_configuration(config, "site_protocols")
  expect_true(is_valid)
})

test_that("validate_configuration() rejects NULL config", {
  is_valid <- validate_configuration(NULL, "site_protocols")
  expect_false(is_valid)
})

test_that("validate_configuration() validates defaults", {
  config <- load_config("defaults")
  skip_if(is.null(config))

  is_valid <- validate_configuration(config, "defaults")
  expect_true(is_valid)
})

test_that("Configuration files have required structure", {
  site_config <- load_config("site_protocols")
  defaults_config <- load_config("defaults")

  skip_if(is.null(site_config))
  skip_if(is.null(defaults_config))

  # Site protocols structure
  expect_true("sites" %in% names(site_config))
  expect_true(length(site_config$sites) >= 2)
  expect_true("NA" %in% names(site_config$sites))

  # Defaults structure
  expect_true("analysis" %in% names(defaults_config))
  expect_true("extraction" %in% names(defaults_config))
})
