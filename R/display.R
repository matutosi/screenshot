#' Get display corner of screen
#'
#' This function returns the coordinates of the specified corner of the display.
#' This function works only on windows.
#'
#' @param size    Integers to specify width or height of display size.
#' @param corner  A string to specify a corner of the display. 
#'                "top_left", "top_right", "bottom_left", or "bottom_right".
#' @param width,height  A integer to specify width or height of the corner.
#' @return A numeric vector of length 4 representing the coordinates of the specified corner.
#' @examples
#' \dontrun{
#' size <- display_size()
#' display_corner(size, "top_left", 800, 800)
#' }
#'
#' @export
display_corner <- function(size, corner = "bottom_left", width = 600, height = 600){
  corner <- 
    switch(corner,
      "top_left"     = c(                 1,                   1, width, height), 
      "top_right"    = c(size$width - width,                   1, width, height), 
      "bottom_left"  = c(                 1, size$height- height, width, height), 
      "bottom_right" = c(size$width - width, size$height- height, width, height),
      stop("corner should be one of ",
           "\"top_left\", \"top_right\", \"bottom_left\" or \"bottom_right\"")
    )
  return(as.integer(corner))
}

#' Get the size of the display.
#' 
#' This function works only on windows.
#' 
#' @return A list with two elements, `width` and `height`, which are the width
#'         and height of the display.
#' @examples
#' \dontrun{
#' display_size()
#' }
#' 
#' @export
display_size <- function(){
  resolution <- display_size_wmic()
  if(length(resolution) < 2){
    # wmic is deprecated and no longer shipped with recent Windows
    resolution <- display_size_powershell()
  }
  return(list(width = resolution[1], height = resolution[2]))
}

#' Helper functions for `display_size()`.
#'
#' @return A numeric vector of width and height, or a shorter vector on failure.
#' @noRd
display_size_wmic <- function(){
  cmd <- paste("wmic path Win32_VideoController get",
               "CurrentHorizontalResolution,CurrentVerticalResolution",
               "/format:value")
  res <- suppressWarnings(try(system(cmd, intern = TRUE), silent = TRUE))
  if(inherits(res, "try-error")){
    return(numeric(0))
  }
  res <- suppressWarnings(as.double(unlist(strsplit(res, "="))))
  return(res[!is.na(res)])
}

#' @noRd
display_size_powershell <- function(){
  cmd <- paste("powershell -NoProfile -Command",
               "\"Add-Type -AssemblyName System.Windows.Forms;",
               "$s = [System.Windows.Forms.Screen]::PrimaryScreen.Bounds;",
               "$s.Width; $s.Height\"")
  res <- suppressWarnings(try(system(cmd, intern = TRUE), silent = TRUE))
  if(inherits(res, "try-error")){
    return(numeric(0))
  }
  res <- suppressWarnings(as.double(res))
  return(res[!is.na(res)])
}
