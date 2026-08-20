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

test_that("screenshot_cmd() quotes a path with a space", {
  # https://github.com/matutosi/screenshot/issues/1
  # A package installed under "C:/Program Files/..." used to fail with
  # "'C:/Program' not found", because the command was not quoted.
  bin_dir <- "C:/Program Files/R/R-4.5.1/library/screenshot"
  file <- "C:/Users/ADMINI~1/AppData/Local/Temp/out file.png"
  cmd <- screenshot_cmd(file, bin_dir, os = "win")
  expect_equal(cmd,
    paste0("\"C:/Program Files/R/R-4.5.1/library/screenshot/screenshot.exe\"",
           " \"C:/Users/ADMINI~1/AppData/Local/Temp/out file.png\""))
})

test_that("screenshot_cmd() quotes for the shell of the platform", {
  file <- "/tmp/out file.png"
  expect_equal(screenshot_cmd(file, os = "mac"),
               "screencapture -o '/tmp/out file.png'")
  expect_equal(screenshot_cmd(file, os = "linux"),
               "gnome-screenshot -f '/tmp/out file.png'")
})

test_that("the quoting style of screenshot_cmd() works on cmd.exe", {
  skip_on_cran()
  skip_if_not(get_os() == "win")
  dir <- fs::path(fs::path_temp(), "Program Test")
  fs::dir_create(dir)
  on.exit(fs::dir_delete(dir))
  bat <- fs::path(dir, "echo.bat")
  writeLines(c("@echo off", "echo ran"), bat)
  # the same style as screenshot_cmd(), with a stand-in for screenshot.exe
  quoted <- paste(shQuote(bat, type = "cmd"),
                  shQuote("out file.png", type = "cmd"))
  expect_equal(suppressWarnings(system(quoted, intern = TRUE)), "ran")
  # what the command used to look like, for comparison
  bare <- paste(bat, "out file.png")
  expect_error(suppressWarnings(system(bare, intern = TRUE)), "not found")
})

test_that("screenshot() warns that quote is deprecated", {
  expect_warning(screenshot(quote = TRUE), "deprecated")
})
