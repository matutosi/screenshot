test_that("screenshot_exists() returns a single logical", {
  exists <- screenshot_exists()
  expect_type(exists, "logical")
  expect_length(exists, 1)
})

test_that("screenshot_exists() reports FALSE for a missing directory on Windows", {
  skip_if_not(get_os() == "win", "bin_dir is used only on Windows")
  expect_false(screenshot_exists(fs::path_temp("no_such_directory")))
})

test_that("screenshot() returns \"\" when the exec file is missing", {
  skip_if_not(get_os() == "win", "bin_dir is used only on Windows")
  expect_message(sc <- screenshot(bin_dir = fs::path_temp("no_such_directory")),
                 "NOT found")
  expect_equal(sc, "")
})

test_that("install_screenshot() does nothing outside Windows", {
  skip_if(get_os() == "win", "the installer runs only on Windows")
  expect_message(path <- install_screenshot(), "screencapture")
  expect_equal(path, "")
})
