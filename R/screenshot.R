#' Take a screenshot. 
#' 
#' Need to install screenCapture on Win by install_screenshot().
#' 
#' @param file  A string for file name of screenshot.
#' @seealso     install_screenshot()
#' @return      A file name of screenshot. When "", screenshot will be saved in a tempral directory.
#' @examples
#' \donttest{
#' library(imager)
#' sc <- screenshot()
#' imager::load.image(sc)
#' }
#' 
#' @export
screenshot <- function(file = ""){
  if(file == ""){
    file <- fs::file_temp("sc_", ext = "png")
  }
  os <- get_os()
  if(os == "win"){
    pkg <- fs::path_package("screenshot")
    exe <- "screenshot.exe"
    cmd <- paste0(fs::path(pkg, exe), " ", file)
  }else if(os == "mac"){
    exe <- "screencapture -o"
    cmd <- paste0(fs::path(exe), " ", file)
  }else{
  }
  system(cmd, intern = TRUE)
  return(file)
}

#' Install command line screenshot for Windows.
#' 
#' Codes are from URL shown below.
#' https://superuser.com/questions/75614/take-a-screen-shot-from-command-line-in-windows#answer-1751844
#' 
#' @param dir A string of direcotory to be installed.
#' @return     A string of installed dir.
#' 
#' @examples
#' \donttest{
#' library(fs)
#' dir <- fs::path_package("screenshot")
#' # dir <- "SET_YOUR DIRECTORY"
#' install_screenshot(dir)
#' }
#' 
#' @export
install_screenshot <- function(dir){
  # directory setting
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(fs::path_temp())
  # download and save
  url <- "https://raw.githubusercontent.com/matutosi/screenshot/main/tools/"
  bat <- "screenshot.bat"
  exe <- "screenshot.exe"
  suppressWarnings({
    paste0(url, bat) %>%
      readLines() %>%
      writeLines(paste0(bat))
  })
  # compile
  system(bat, intern = TRUE)
  # move
  sc_exe <- fs::path(dir, exe)
  fs::file_move(exe, sc_exe)
  message(exe, " is installed in ", dir)
  return(sc_exe)
}
