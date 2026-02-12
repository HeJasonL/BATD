test_that("%ni% operator works correctly - basic negation", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4)
  
  result <- x %ni% y
  expected <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  
  expect_equal(result, expected)
})

test_that("%ni% operator with strings", {
  sites <- c("UCLA", "KKI", "CCH", "SPIN")
  known_sites <- c("UCLA", "KKI")
  
  unknown <- sites %ni% known_sites
  expected_sites <- c("CCH", "SPIN")
  
  expect_equal(sites[unknown], expected_sites)
})

test_that("%ni% operator returns all TRUE when no matches", {
  x <- c("a", "b", "c")
  y <- c("x", "y", "z")
  
  result <- x %ni% y
  
  expect_true(all(result))
})

test_that("%ni% operator returns all FALSE when all match", {
  x <- c("a", "b", "c")
  y <- c("a", "b", "c", "d", "e")
  
  result <- x %ni% y
  
  expect_false(any(result))
})

test_that("%ni% operator is exported from package", {
  exports <- getNamespaceExports("BATD")
  
  expect_true("%ni%" %in% exports)
})

test_that("%ni% operator handles numeric vectors", {
  protocols <- c(100, 101, 102, 103, 171)
  excluded <- c(171, 200)
  
  result <- protocols %ni% excluded
  expected <- c(TRUE, TRUE, TRUE, TRUE, FALSE)
  
  expect_equal(result, expected)
})
