test_that("index2xy() converts a matrix index into row and column", {
  nrow <- 4
  mt <- matrix(1:12, nrow = nrow)
  for(index in seq_along(mt)){
    xy <- index2xy(index, nrow)
    expect_equal(mt[xy[1], xy[2]], index)
  }
})

test_that("index2xy() keeps the index on the last row in the same column", {
  # the last row of a column has index %% nrow == 0
  expect_equal(index2xy(4,  4), c(4, 1))
  expect_equal(index2xy(8,  4), c(4, 2))
  expect_equal(index2xy(12, 4), c(4, 3))
})

test_that("xy_pos() returns every position holding the value", {
  mt <- matrix(c(1, 2, 3, 2), nrow = 2)
  expect_equal(xy_pos(mt, 5), list())
  expect_equal(xy_pos(mt, 1), list(c(1, 1)))
  expect_equal(xy_pos(mt, 2), list(c(2, 1), c(2, 2)))
  # every reported position really holds the value
  for(xy in xy_pos(mt, 2)){
    expect_equal(mt[xy[1], xy[2]], 2)
  }
})
