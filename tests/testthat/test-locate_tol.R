test_that("locate_ndl_in_hay() needs an exact match by default", {
  haystack_image <- imager::load.example("parrots")
  hay_mt <- image2gray_matrix(haystack_image)
  ndl_mt <- image2gray_matrix(crop_image(haystack_image, 129, 257, w = 60, h = 40))
  noisy <- ndl_mt + 1 / 255
  expect_message(pos <- locate_ndl_in_hay(noisy, hay_mt), "Not found")
  expect_equal(pos, c(0, 0))
})

test_that("locate_ndl_in_hay() finds a needle within the tolerance", {
  haystack_image <- imager::load.example("parrots")
  hay_mt <- image2gray_matrix(haystack_image)
  ndl_mt <- image2gray_matrix(crop_image(haystack_image, 129, 257, w = 60, h = 40))
  noisy <- ndl_mt + 1 / 255
  expect_equal(locate_ndl_in_hay(noisy, hay_mt, tol = 1), c(129, 257))
  expect_equal(locate_ndl_in_hay(noisy, hay_mt, tol = 4), c(129, 257))
})

test_that("locate_ndl_in_hay() survives a lossy format with a tolerance", {
  haystack_image <- imager::load.example("parrots")
  hay_mt <- image2gray_matrix(haystack_image)
  needle_image <- crop_image(haystack_image, 129, 257, w = 100, h = 50)
  path <- fs::path_temp(ext = "jpg")
  on.exit(try(fs::file_delete(path), silent = TRUE))
  imager::save.image(needle_image, path, quality = 0.95)
  ndl_mt <- image2gray_matrix(imager::load.image(path))
  expect_equal(locate_ndl_in_hay(ndl_mt, hay_mt, tol = 12), c(129, 257))
})

test_that("locate_ndl_in_hay() reports a needle larger than the haystack", {
  hay_mt <- matrix(seq(0, 1, length.out = 25), nrow = 5)
  ndl_mt <- matrix(seq(0, 1, length.out = 100), nrow = 10)
  expect_message(pos <- locate_ndl_in_hay(ndl_mt, hay_mt), "larger")
  expect_equal(pos, c(0, 0))
})

test_that("locate_ndl_in_hay() compares sampled pixels when exact is FALSE", {
  haystack_image <- imager::load.example("parrots")
  hay_mt <- image2gray_matrix(haystack_image)
  ndl_mt <- image2gray_matrix(crop_image(haystack_image, 129, 257, w = 60, h = 40))
  expect_equal(locate_ndl_in_hay(ndl_mt, hay_mt, exact = FALSE), c(129, 257))
})

test_that("tol2value() keeps a difference of one step inside the tolerance", {
  # a grayscale value is a weighted sum of RGB, so 1/255 is not exact
  expect_equal(tol2value(0), 0)
  expect_equal(tol2value(-1), 0)
  expect_true(tol2value(1) > 1 / 255)
  expect_true(tol2value(1) < 2 / 255)
})

test_that("probe_points() stays inside the needle and covers its corners", {
  probe <- probe_points(50, 20)
  expect_true(all(probe[, 1] >= 1 & probe[, 1] <= 50))
  expect_true(all(probe[, 2] >= 1 & probe[, 2] <= 20))
  expect_true(any(probe[, 1] == 1  & probe[, 2] == 1))
  expect_true(any(probe[, 1] == 50 & probe[, 2] == 20))
  # a needle of one pixel gives one probe
  expect_equal(nrow(probe_points(1, 1)), 1)
})

test_that("anchor_value() picks a value shared by both matrices", {
  ndl_mt <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  hay_mt <- matrix(c(0.1, 0.1, 0.1, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), nrow = 3)
  # 0.4 appears once in the haystack, 0.1 three times
  expect_equal(anchor_value(ndl_mt, hay_mt), 0.4)
  expect_null(anchor_value(matrix(c(-1, -2), nrow = 1), hay_mt))
})
