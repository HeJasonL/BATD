test_that("BATD_extract_NF backward compatibility - function exists", {
  expect_true(exists("BATD_extract_NF"))
  expect_is(BATD_extract_NF, "function")
})

test_that("BATD_extract_OF backward compatibility - function exists", {
  expect_true(exists("BATD_extract_OF"))
  expect_is(BATD_extract_OF, "function")
})

test_that("BATD_extract_NF has correct parameter signature", {
  sig <- formals(BATD_extract_NF)
  
  expect_true("list_of_filenames" %in% names(sig))
  expect_true("site" %in% names(sig))
})

test_that("BATD_extract_OF has correct parameter signature", {
  sig <- formals(BATD_extract_OF)
  
  expect_true("list_of_filenames" %in% names(sig))
  expect_true("Site" %in% names(sig))
})

test_that("BATD_extract_NF is exported from package", {
  exports <- getNamespaceExports("BATD")
  
  expect_true("BATD_extract_NF" %in% exports)
})

test_that("BATD_extract_OF is exported from package", {
  exports <- getNamespaceExports("BATD")
  
  expect_true("BATD_extract_OF" %in% exports)
})

test_that("BATD_extract_NF documentation mentions format auto-detection", {
  help_text <- utils::capture.output(help(BATD_extract_NF, package = "BATD"))
  
  expect_true(any(grepl("BATD_extract", help_text)))
  expect_true(any(grepl("auto", tolower(help_text))))
})

test_that("BATD_extract_OF documentation mentions format auto-detection", {
  help_text <- utils::capture.output(help(BATD_extract_OF, package = "BATD"))
  
  expect_true(any(grepl("BATD_extract", help_text)))
  expect_true(any(grepl("auto", tolower(help_text))))
})
