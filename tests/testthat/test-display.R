test_that("get_os() returns one of the supported names", {
  expect_true(get_os() %in% c("win", "linux", "mac"))
  expect_length(get_os(), 1)
})

test_that("display_corner() returns the four corners of the display", {
  size <- list(width = 1920, height = 1080)
  expect_equal(display_corner(size, "top_left",     800, 600), c(1, 1, 800, 600))
  expect_equal(display_corner(size, "top_right",    800, 600), c(1120, 1, 800, 600))
  expect_equal(display_corner(size, "bottom_left",  800, 600), c(1, 480, 800, 600))
  expect_equal(display_corner(size, "bottom_right", 800, 600), c(1120, 480, 800, 600))
})

test_that("display_corner() returns four integers", {
  size <- list(width = 1920, height = 1080)
  corner <- display_corner(size)
  expect_type(corner, "integer")
  expect_length(corner, 4)
})

test_that("display_corner() rejects an unknown corner name", {
  size <- list(width = 1920, height = 1080)
  expect_error(display_corner(size, "center"), "corner should be one of")
})
