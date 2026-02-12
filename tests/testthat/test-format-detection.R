test_that("detect_file_format identifies NF format from file content", {
  filepath <- system.file("tests/fixtures/sample_nf_minimal.txt", package = "BATD")
  
  result <- BATD:::detect_file_format(filepath)
  
  expect_equal(result$format, "NF")
  expect_gte(result$confidence, 0.7)
  expect_type(result$reasoning, "character")
})

test_that("detect_file_format returns error for non-existent file", {
  expect_error(
    BATD:::detect_file_format("/non/existent/path.txt"),
    "File or directory not found"
  )
})

test_that("detect_file_format handles folders for OF format detection", {
  # Create a temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  result <- BATD:::detect_file_format(temp_dir)
  
  expect_equal(result$format, "OF")
  expect_equal(result$confidence, 0.95)
  
  # Cleanup
  unlink(temp_dir)
})

test_that("validate_file_format issues warning on mismatch when not strict", {
  filepath <- system.file("tests/fixtures/sample_nf_minimal.txt", package = "BATD")
  
  expect_warning(
    result <- BATD:::validate_file_format(filepath, "OF", strict = FALSE),
    "Format mismatch"
  )
  
  expect_false(result)
})

test_that("validate_file_format stops on mismatch when strict = TRUE", {
  filepath <- system.file("tests/fixtures/sample_nf_minimal.txt", package = "BATD")
  
  expect_error(
    BATD:::validate_file_format(filepath, "OF", strict = TRUE),
    "Format mismatch"
  )
})

test_that("validate_file_format returns TRUE on match", {
  filepath <- system.file("tests/fixtures/sample_nf_minimal.txt", package = "BATD")
  
  result <- BATD:::validate_file_format(filepath, "NF", strict = FALSE)
  
  expect_true(result)
})
