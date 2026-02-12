test_that("normalize_demographics converts demographic values correctly", {
  # Test NF format (Male, Female, Right, Left, caucasian)
  nf_data <- data.frame(
    id = "P001",
    race = "caucasian",
    gender = "Male",
    handedness = "Right",
    birthYear = "1990",
    stringsAsFactors = FALSE
  )
  
  result <- BATD:::normalize_demographics(nf_data)
  
  expect_equal(result$gender, "Male")
  expect_equal(result$handedness, "Right")
  expect_equal(result$race, "caucasian")
})

test_that("normalize_demographics converts OF format (M/F/R/L) correctly", {
  # Test OF format (M, F, R, L, Default)
  of_data <- data.frame(
    id = "P002",
    race = "Default",
    gender = "M",
    handedness = "R",
    birthYear = "1985",
    stringsAsFactors = FALSE
  )
  
  result <- BATD:::normalize_demographics(of_data)
  
  expect_equal(result$gender, "Male")
  expect_equal(result$handedness, "Right")
  expect_equal(result$race, "caucasian")
})

test_that("normalize_demographics handles Female handedness Left", {
  of_data <- data.frame(
    id = "P003",
    race = "Default",
    gender = "F",
    handedness = "L",
    birthYear = "1988",
    stringsAsFactors = FALSE
  )
  
  result <- BATD:::normalize_demographics(of_data)
  
  expect_equal(result$gender, "Female")
  expect_equal(result$handedness, "Left")
})

test_that("normalize_demographics preserves non-default race values", {
  of_data <- data.frame(
    id = "P004",
    race = "African American",
    gender = "M",
    handedness = "R",
    birthYear = "1992",
    stringsAsFactors = FALSE
  )
  
  result <- BATD:::normalize_demographics(of_data)
  
  expect_equal(result$race, "African American")
})
