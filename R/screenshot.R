#' Take a screenshot.
#'
#' Need to install screenshot.exe on Win by install_screenshot().
#'
#' @param file     A string for file name of screenshot.
#' @param bin_dir  A string for directory name of screenshot.exe on Win.
#' @seealso        install_screenshot()
#' @return         A file name of screenshot. When "", screenshot will be saved in a tempral directory.
#' @examples
#' if(interactive()){
#'
#' sc <- screenshot()
#' if(sc != ""){
#'   sc_image <- imager::load.image(sc)
#'   plot(sc_image)
#' }
#'
#' }
#'
#' @export
screenshot <- function(file = "", bin_dir = ""){
  if(!screenshot_exists(bin_dir)){
    message("NOT found screenshot exec file")
    return("")
  }
  if(file == ""){
    file <- fs::file_temp("sc_", ext = "png")
  }
  os <- get_os()
  if(os == "win"){
    if(bin_dir == ""){
      bin_dir <- fs::path_package("screenshot")
    }
    exe <- "screenshot.exe"
    cmd <- paste0(fs::path(bin_dir, exe), " ", file)
  }else if(os == "mac"){
    exe <- "screencapture -o"
    cmd <- paste0(fs::path(exe), " ", file)
  }else{
  # maybe Linux (almost GNOME?)
    exe <- "gnome-screenshot -f"
    cmd <- paste0(exe, " ", file)
  }
  system(cmd, intern = TRUE)
  return(file)
}

#' Install command line screenshot for Windows.
#'
#' Codes are from URL shown below.
#'   https://superuser.com/questions/75614/take-a-screen-shot-from-command-line-in-windows#answer-1751844
#' On Mac `screencapture` is usually available.
#' On Linux GNOME desktop use `gnome-screenshot`.
#' If not installed, run `sudo apt install gnome-screenshot`.
#'
#' @param bin_dir A string of directory to be installed.
#' @return        A string of installed directory.
#'
#' @examples
#' if(interactive()){
#'
#' # need only on Win
#' if(get_os() == "win"){
#'   bin_dir <- fs::path_package("screenshot")
#'   # if you want to install another directory
#'   #   bin_dir <- "SET_YOUR DIRECTORY"
#'   install_screenshot(bin_dir)
#' }
#'
#' }
#'
#' @export
install_screenshot <- function(bin_dir = ""){
  os <- get_os()
  if(os != "win"){
    message("On Mac `screencapture` is usually available.")
    message("On Linux GNOME desktop use `gnome-screenshot`.")
    message("If not installed, run `sudo apt install gnome-screenshot`.")
    return("")
  }
  # directory setting
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(fs::path_temp())
  # download and save
  url <- "https://raw.githubusercontent.com/matutosi/screenshot/main/tools/"
  bat <- "screenshot.bat"
  exe <- "screenshot.exe"
  suppressWarnings({
    paste0(url, bat) |>
      readLines() |>
      writeLines(paste0(bat))
  })
  # compile
  system(bat, intern = TRUE)
  # move
  if(bin_dir == ""){
    bin_dir <- fs::path_package("screenshot")
  }
  sc_exe <- fs::path(bin_dir, exe)
  fs::file_move(exe, sc_exe)
  message(exe, " is installed in \n", bin_dir)
  return(invisible(sc_exe))
}

#' Find screenshot exec file.
#'
#' @param bin_dir  A string for directory name screenshot.exe exec file.
#'                 No need on Mac and Linux.
#' @return     A logical.
#'
#' @examples
#' screenshot_exists()
#'
#' @export
screenshot_exists <- function(bin_dir = ""){
  os <- get_os()
  if(os == "win"){
    wd <- getwd()
    on.exit(setwd(wd))
    if(bin_dir == ""){
      bin_dir <- fs::path_package("screenshot")
    }
    setwd(bin_dir)
    exists <- fs::file_exists("screenshot.exe")
  }else if(os == "mac"){
    exe <- "screencapture"
    exists <- Sys.which(exe) != ""
  }else{
    exe <- "gnome-screenshot"
    exists <- Sys.which(exe) != ""
  }
  return(exists)
}
