test_that("crop_image() cuts the requested area", {
  image <- imager::load.example("parrots")
  w <- 100
  h <- 50
  croped <- crop_image(image, 200, 250, w, h)
  expect_s3_class(croped, "cimg")
  expect_equal(imager::width(croped), w)
  expect_equal(imager::height(croped), h)
  expect_equal(imager::spectrum(croped), imager::spectrum(image))
  # the cut area holds the same pixels as the original
  expect_equal(as.numeric(croped[1, 1, 1, 1]), as.numeric(image[200, 250, 1, 1]))
  expect_equal(as.numeric(croped[w, h, 1, 1]),
               as.numeric(image[200 + w - 1, 250 + h - 1, 1, 1]))
})

test_that("crop_image() reaches the bottom right corner of the image", {
  image <- imager::load.example("parrots")
  w <- 10
  h <- 10
  pos_x <- imager::width(image)  - w + 1
  pos_y <- imager::height(image) - h + 1
  expect_no_error(crop_image(image, pos_x, pos_y, w, h))
})

test_that("crop_image() reports an area outside of the image", {
  image <- imager::load.example("parrots")
  expect_error(crop_image(image, 0, 1, 10, 10), "outside of the image")
  expect_error(crop_image(image, imager::width(image), 1, 10, 10),
               "outside of the image")
  expect_error(crop_image(image, 1, imager::height(image), 10, 10),
               "outside of the image")
})

test_that("hay2needle() is deprecated but keeps working", {
  image <- imager::load.example("parrots")
  expect_warning(needle <- hay2needle(image, 200, 250, 100, 50), "deprecated")
  expect_equal(dim(needle), dim(crop_image(image, 200, 250, 100, 50)))
  # the defaults of crop_image() are still available
  expect_warning(needle <- hay2needle(image, 200, 250), "deprecated")
  expect_equal(imager::width(needle), 50)
  expect_equal(imager::height(needle), 20)
})

test_that("image2gray_matrix() returns a matrix of the image size", {
  image <- imager::load.example("parrots")
  mt <- image2gray_matrix(image)
  expect_true(is.matrix(mt))
  expect_equal(nrow(mt), imager::width(image))
  expect_equal(ncol(mt), imager::height(image))
})
