#' Take a screencapture (screenshot). 
#' 
#' Need to install screenCapture on Win by install_screencapture().
#' 
#' @param file  A string for file name of screencapture.
#' @seealso     install_screencapture()
#' @return      A file name of screencapture. When "", screencapture will be saved in a tempral directory.
#' @examples
#' \donttest{
#' library(imager)
#' sc <- screencapture()
#' imager::load.image(sc)
#' }
#' 
#' @export
screencapture <- function(file = ""){
  if(file == ""){
    file <- fs::file_temp("sc_", ext = "png")
  }
  os <- get_os()
  if(os == "win"){
    pkg <- fs::path_package("screencapture")
    exe <- "screencapture.exe"
    cmd <- paste0(fs::path(pkg, exe), " ", file)
  }else if(os == "mac"){
    exe <- "screencapture -o"
    cmd <- paste0(fs::path(exe), " ", file)
  }else{
  }
  system(cmd, intern = TRUE)
  return(file)
}

#' Install command line screencapture (screenshot) for Windows.
#' 
#' Codes are from URL shown below.
#' https://superuser.com/questions/75614/take-a-screen-shot-from-command-line-in-windows#answer-1751844
#' 
#' @param paht A string of direcotory to be installed.
#' @return     A string of installed dir.
#' 
#' @examples
#' \donttest{
#' library(fs)
#' dir <- fs::path_package("screencapture")
#' # dir <- "SET_YOUR DIRECTORY"
#' install_screencapture(dir)
#' }
#' 
#' @export
install_screencapture <- function(dir){
  # directory setting
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(fs::path_temp())
  # download and save
  url <- "https://raw.githubusercontent.com/matutosi/screencapture/main/tools/"
  bat <- "screencapture.bat"
  exe <- "screencapture.exe"
  suppressWarnings({
    paste0(url, bat) %>%
      readLines() %>%
      writeLines(paste0(bat))
  })
  # compile
  system(bat, intern = TRUE)
  # move
  sc_exe <- paste0(dir, "/", exe)
  file.rename(exe, sc_exe)
  message(exe, " is installed in ", dir)
  return(sc_exe)
}
