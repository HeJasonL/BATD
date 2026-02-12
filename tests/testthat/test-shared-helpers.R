test_that("standardize_column_types converts numeric columns correctly", {
  # Create a mock extraction output with mixed types
  data <- data.frame(
    id = "P001",
    protocol = "100",
    numberofPracticeTrials = "5",
    numberofTestTrials = "20",
    ISI = "100",
    stim1amplitude = "50",
    trialNumber = "1",
    value = "50",
    expected = "1",
    response = "1",
    correctResponse = "true",
    responseTime = "450",
    stringsAsFactors = FALSE
  )
  
  result <- BATD:::standardize_column_types(data)
  
  expect_type(result$protocol, "character")
  expect_type(result$id, "character")
})

test_that("standardize_column_types handles NA values", {
  data <- data.frame(
    id = "P001",
    numberofPracticeTrials = NA,
    numberofTestTrials = "20",
    stringsAsFactors = FALSE
  )
  
  result <- BATD:::standardize_column_types(data)
  
  expect_true(is.na(result$numberofPracticeTrials[1]))
})

test_that("standardize_column_types fills NA practice trials with 0", {
  data <- data.frame(
    id = "P001",
    numberofPracticeTrials = NA,
    numberofTestTrials = "20",
    stringsAsFactors = FALSE
  )
  
  result <- BATD:::standardize_column_types(data)
  
  # After standardization, NA values in numberofPracticeTrials should be 0
  expect_equal(result$numberofPracticeTrials[1], 0)
})

test_that("account_for_repeated_runs adds run column", {
  # Create mock data with repeated protocols
  data <- data.frame(
    id = c("P001", "P001", "P001", "P001"),
    session = c(1, 1, 1, 1),
    time = c("10:00:00", "10:00:00", "11:00:00", "11:00:00"),
    protocolName = c("Amplitude Discrimination", "Amplitude Discrimination", 
                     "Amplitude Discrimination", "Amplitude Discrimination"),
    stringsAsFactors = FALSE
  )
  
  result <- BATD:::account_for_repeated_runs(data)
  
  expect_true("run" %in% names(result))
})

test_that("account_for_repeated_runs assigns correct run numbers", {
  data <- data.frame(
    id = c("P001", "P001", "P001", "P001"),
    session = c(1, 1, 1, 1),
    time = c("10:00:00", "10:00:00", "11:00:00", "11:00:00"),
    protocolName = c("Amplitude Discrimination", "Amplitude Discrimination", 
                     "Amplitude Discrimination", "Amplitude Discrimination"),
    stringsAsFactors = FALSE
  )
  
  result <- BATD:::account_for_repeated_runs(data)
  
  # First two rows should have run 1, next two should have run 2
  expect_equal(result$run[1:2], c(1, 1))
  expect_equal(result$run[3:4], c(2, 2))
})

test_that("validate_extracted_data checks for required columns", {
  # Valid data
  valid_data <- data.frame(
    id = "P001",
    date = "2024-01-15",
    time = "10:00:00",
    protocol = 100,
    protocolName = "Amplitude Discrimination",
    value = 50,
    trialNumber = 1,
    correctResponse = 1,
    stringsAsFactors = FALSE
  )
  
  expect_error(BATD:::validate_extracted_data(valid_data), NA)
})

test_that("validate_extracted_data fails with missing required columns", {
  # Missing protocolName
  invalid_data <- data.frame(
    id = "P001",
    date = "2024-01-15",
    time = "10:00:00",
    protocol = 100,
    value = 50,
    trialNumber = 1,
    correctResponse = 1,
    stringsAsFactors = FALSE
  )
  
  expect_error(
    BATD:::validate_extracted_data(invalid_data),
    "Required columns missing"
  )
})
