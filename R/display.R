#' Get display corner of screen
#'
#' This function returns the coordinates of the specified corner of the display.
#' This function works only on windows.
#'
#' @param corner  A string to specify a corner of the display. 
#'                "top_left", "top_right", "bottom_left", or "bottom_right".
#' @param width,height  A integer to specify width or height of the corner.
#' @return A numeric vector of length 4 representing the coordinates of the specified corner.
#' @examples
#' \dontrun{
#' display_corner("top_left", 800, 800)
#' }
#'
#' @export
display_corner <- function(corner = "bottom_left", width = 600, height = 600){
  size <- display_size()
  corner <- 
    switch(corner,
      "top_left"     = c(                 1,                   1, width, height), 
      "top_right"    = c(size$width - width,                   1, width, height), 
      "bottom_left"  = c(                 1, size$height- height, width, height), 
      "bottom_right" = c(size$width - width, size$height- height, width, height)
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
  suppressWarnings(
    resolution <- 
      "wmic path Win32_VideoController get CurrentHorizontalResolution,CurrentVerticalResolution /format:value" |>
      system(intern = TRUE) |>
      strsplit("=") |>
      unlist() |>
      as.double()
  )
  resolution <- resolution[!is.na(resolution)]
  return(list(width = resolution[1], height = resolution[2]))
}
