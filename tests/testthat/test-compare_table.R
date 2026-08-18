test_that("count_val_freq() counts each value once", {
  mt <- matrix(c(1, 1, 2, 3, 3, 3), nrow = 2)
  freq <- count_val_freq(mt, "n")
  expect_equal(freq$val, c(1, 2, 3))
  expect_equal(freq$n, c(2, 1, 3))
  expect_equal(sum(freq$n), length(mt))
})

test_that("compare_table() joins the counts of both matrices", {
  ndl_mt <- matrix(c(1, 2, 2), nrow = 1)
  hay_mt <- matrix(c(1, 2, 2, 2, 9, 9), nrow = 2)
  comp <- expect_no_message(compare_table(ndl_mt, hay_mt))
  expect_equal(comp$val, c(1, 2))
  expect_equal(comp$ndl, c(1, 2))
  expect_equal(comp$hay, c(1, 3))
})

test_that("compare_table() sorts the rarest value in the haystack first", {
  # locate_ndl_in_hay() relies on this order to start from the fewest candidates
  ndl_mt <- matrix(c(1, 2, 3), nrow = 1)
  hay_mt <- matrix(c(3, 3, 3, 2, 2, 1), nrow = 2)
  comp <- compare_table(ndl_mt, hay_mt)
  expect_equal(comp$val, c(1, 2, 3))
  expect_false(is.unsorted(comp$hay))
})

test_that("compare_table() keeps needle values missing from the haystack", {
  ndl_mt <- matrix(c(1, 7), nrow = 1)
  hay_mt <- matrix(c(1, 1), nrow = 1)
  comp <- compare_table(ndl_mt, hay_mt)
  expect_true(7 %in% comp$val)
  expect_true(is.na(comp$hay[comp$val == 7]))
})
