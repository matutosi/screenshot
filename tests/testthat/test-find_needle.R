testthat::test_that("locate_position() can find needle image", {
  located_position_of_needle_image_by_locate_position_function <- function(example, pos_x, pos_y){
    haystack_image <- imager::load.example(example)
    hay_mt <- image2gray_matrix(haystack_image)
    ndl_mt <- 
      haystack_image |>
      hay2needle(pos_x, pos_y) |>
      image2gray_matrix()
    xy <- locate_ndl_in_hay(ndl_mt, hay_mt, exact = FALSE)
    return(xy)
  }

  examples <- c("hubble", "birds", "parrots")
  positions <- c(101, 201, 301)
  for(example in examples){
    for(pos in positions){
    image <- imager::load.example(example)
    expect_equal(
      c(pos, pos),
      located_position_of_needle_image_by_locate_position_function(example, pos, pos))
    }
  }

})
