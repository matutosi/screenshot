test_that("locate_ndl_in_hay() finds the needle image", {
  locate_cropped_needle <- function(example, pos_x, pos_y, exact = TRUE){
    haystack_image <- imager::load.example(example)
    hay_mt <- image2gray_matrix(haystack_image)
    ndl_mt <-
      haystack_image |>
      crop_image(pos_x, pos_y, w = 50, h = 20) |>
      image2gray_matrix()
    locate_ndl_in_hay(ndl_mt, hay_mt, exact = exact)
  }

  examples <- c("hubble", "birds", "parrots")
  positions <- c(101, 201, 301)
  for(example in examples){
    for(pos in positions){
      expect_equal(locate_cropped_needle(example, pos, pos, exact = FALSE),
                   c(pos, pos))
    }
  }
})

test_that("locate_ndl_in_hay() finds the needle image with an exact match", {
  haystack_image <- imager::load.example("parrots")
  hay_mt <- image2gray_matrix(haystack_image)
  ndl_mt <-
    haystack_image |>
    crop_image(129, 257, w = 100, h = 50) |>
    image2gray_matrix()
  expect_equal(locate_ndl_in_hay(ndl_mt, hay_mt, exact = TRUE), c(129, 257))
})

test_that("locate_ndl_in_hay() finds a needle at the edge of the haystack", {
  haystack_image <- imager::load.example("parrots")
  w <- 30
  h <- 20
  pos_x <- imager::width(haystack_image)  - w + 1
  pos_y <- imager::height(haystack_image) - h + 1
  hay_mt <- image2gray_matrix(haystack_image)
  ndl_mt <-
    haystack_image |>
    crop_image(pos_x, pos_y, w = w, h = h) |>
    image2gray_matrix()
  expect_equal(locate_ndl_in_hay(ndl_mt, hay_mt, exact = TRUE), c(pos_x, pos_y))
})

test_that("locate_ndl_in_hay() reports a needle absent from the haystack", {
  hay_mt <- matrix(seq(0, 1, length.out = 100), nrow = 10)
  ndl_mt <- matrix(c(-1, -2, -3, -4), nrow = 2)
  expect_message(pos <- locate_ndl_in_hay(ndl_mt, hay_mt, exact = TRUE))
  expect_equal(pos, c(0, 0))
})
