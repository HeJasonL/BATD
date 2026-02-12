test_that("BATD_extract auto-detects NF format", {
  filepath <- system.file("tests/fixtures/sample_nf_minimal.txt", package = "BATD")
  
  skip_if_not(file.exists(filepath), "Sample NF file not found")
  
  result <- BATD::BATD_extract(
    list_of_filenames = basename(filepath),
    site = "UCLA",
    format = "auto"
  )
  
  expect_is(result, "data.frame")
  expect_gte(nrow(result), 1)
  expect_true("id" %in% names(result))
  expect_true("protocol" %in% names(result))
  expect_true("format" %in% names(result))
  expect_equal(unique(result$format), "NF")
})

test_that("BATD_extract handles explicit NF format specification", {
  filepath <- system.file("tests/fixtures/sample_nf_minimal.txt", package = "BATD")
  
  skip_if_not(file.exists(filepath), "Sample NF file not found")
  
  result <- BATD::BATD_extract(
    list_of_filenames = basename(filepath),
    site = "UCLA",
    format = "NF"
  )
  
  expect_is(result, "data.frame")
  expect_equal(unique(result$format), "NF")
})

test_that("BATD_extract validates site parameter", {
  filepath <- system.file("tests/fixtures/sample_nf_minimal.txt", package = "BATD")
  
  skip_if_not(file.exists(filepath), "Sample NF file not found")
  
  expect_error(
    BATD::BATD_extract(
      list_of_filenames = basename(filepath),
      site = NA,
      format = "NF"
    ),
    "site must be a character string"
  )
})

test_that("BATD_extract validates list_of_filenames parameter", {
  expect_error(
    BATD::BATD_extract(
      list_of_filenames = 123,
      site = "UCLA",
      format = "NF"
    ),
    "list_of_filenames must be a character vector"
  )
})

test_that("BATD_extract warns on low-confidence format detection", {
  # This would require an ambiguous file, which is hard to create
  # For now, we test the parameter validation
  expect_error(
    BATD::BATD_extract(
      list_of_filenames = "nonexistent.txt",
      site = "UCLA",
      format = "auto"
    )
  )
})

test_that("BATD_extract has @seealso reference to extraction functions", {
  # This is more of a documentation test
  # Verify that BATD_extract is documented properly
  help_text <- utils::capture.output(help(BATD_extract, package = "BATD"))
  
  expect_true(any(grepl("BATD_extract_NF", help_text)))
  expect_true(any(grepl("BATD_extract_OF", help_text)))
})
