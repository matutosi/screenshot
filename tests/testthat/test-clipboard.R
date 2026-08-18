test_that("hex2little_endian() reverses the byte order", {
  expect_equal(hex2little_endian("01234567"), c("67", "45", "23", "01"))
  expect_equal(hex2little_endian("012345"),   c("45", "23", "01", "00"))
})

test_that("hex2little_endian() left-pads an odd number of digits", {
  # "123" is 0x000123, so the bytes are 23 01 00 00
  expect_equal(hex2little_endian("123"), c("23", "01", "00", "00"))
  expect_equal(hex2little_endian("1"),   c("01", "00", "00", "00"))
})

test_that("hex2little_endian() round-trips a hexmode value", {
  for(value in c(1L, 66L, 291L, 4660L, 246306L)){
    bytes <- hex2little_endian(as.hexmode(value))
    back  <- strtoi(paste0(rev(bytes), collapse = ""), base = 16L)
    expect_equal(back, value)
  }
})

test_that("hex2little_endian() rejects a value wider than 4 bytes", {
  expect_error(hex2little_endian("012345678"), "Too big size")
})

test_that("create_header() writes a 14 byte BMP header", {
  data(clipboard_sample)
  header <- create_header(clipboard_sample)
  expect_type(header, "raw")
  expect_length(header, 14)
  expect_equal(header[1:2], as.raw(c(0x42, 0x4d))) # "BM"
  expect_equal(header[11:14], as.raw(c(0x42, 0, 0, 0))) # offset to the pixels
})

test_that("create_header() records the whole file size", {
  data(clipboard_sample)
  header <- create_header(clipboard_sample)
  size <- sum(as.integer(header[3:6]) * 256^(0:3))
  expect_equal(size, length(clipboard_sample) + length(header))
})

test_that("save_bmp() writes the given bytes and returns the path", {
  data(clipboard_sample)
  image_data <- c(create_header(clipboard_sample), clipboard_sample)
  path <- fs::path_temp(ext = "bmp")
  on.exit(try(fs::file_delete(path), silent = TRUE))
  expect_equal(save_bmp(image_data, path), path)
  expect_true(fs::file_exists(path))
  expect_equal(as.numeric(fs::file_size(path)), length(image_data))
})
