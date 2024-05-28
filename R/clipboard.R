#' Saves an image from the clipboard to a file
#'
#' This function works only on windows.
#'
#' @param path Optional path to save the image to.
#'             If not specified, a temporary file will be created.
#'
#' @return The path to the saved image file.
#'
#' @examples
#' \dontrun{
#' # Save the image from the clipboard to a file
#' save_clipboard_image("clipboard_image.png")
#' }
#'
#' @export
save_clipboard_image <- function(path = ""){
  path_bmp <- clipboard2bitmap()
  path_png <- bitmap2png(path_bmp)
  if(path != ""){
    path <- fs::file_move(path_png, path)
  }else{
    path <- path_png
  }
  return(path)
}

#' Converts a bitmap image to PNG using ImageMagick's convert command
#'
#' @param path Path to the bitmap image.
#'
#' @return The result of the system call.
#'
#' @examples
#' \dontrun{
#' bitmap2png("path/to/image.bmp")
#' }
#'
#' @export
bitmap2png <- function(path){
  out <- fs::path_ext_set(path, "png")
  imager::load.image(path)|>
    imager::save.image(out)
  return(out)
}

#' Save clipboard image to temporary BMP file
#'
#' This function works only on windows.
#' This function saves the image currently in the clipboard
#' to a temporary BMP file.
#'
#' @return Path to the temporary BMP file.
#' @examples
#' \dontrun{
#' clipboard2bitmap()
#' }
#' @export
clipboard2bitmap <- function(){
  clipboard <- get_clipboard_image()
  clipboard[17:20] <- raw(4)
  header <- create_header(clipboard)
  image_data <- c(header, clipboard)
  path <- fs::path_temp(ext = "bmp")
  save_bmp(image_data, path)
  return(path)
}

#' Save an image as a BMP file
#'
#' @param image_data A raster image data object, such as an array
#'        of pixel values or an R object representing an image.
#' @param path The path to the file to be saved.
#' @return Saves the image as a BMP file at the specified path.
#' @examples
#' \dontrun{
#' # Create an image data object
#' image_data <- matrix(rnorm(100), ncol = 10)
#' # Save the image as a BMP file
#' save_bmp(image_data, "image.bmp")
#' }
#'
#' @export
save_bmp <- function(image_data, path){
  con <- file(path, "wb")
    writeBin(image_data, con)
  close(con)
  return(path)
}

#' Convert hexadecimal string to little-endian
#'
#' @param x Hexadecimal string
#' @return Little-endian hexadecimal string
#' @examples
#' hex2little_endian("01234567")
#' hex2little_endian("012345")
#'
#' @export
hex2little_endian <- function(x){
  len <- stringr::str_length(x)
  if(len > 8){
    stop("Too big size")
  }
  if((len %% 2) == 1){
    x <- paste0(stringr::str_sub(x, 1, len-1), "0", stringr::str_sub(x, len))
  }
  x <- stringr::str_pad(x, width = 8, side = "left", pad = "0")
  x <- c(stringr::str_sub(x, 7, 8),
         stringr::str_sub(x, 5, 6),
         stringr::str_sub(x, 3, 4),
         stringr::str_sub(x, 1, 2))
  return(x)
}

#' Retrieves the image from the clipboard
#'
#' This function works only on windows.
#'
#' @return A raw vector containing the image data.
#' @examples
#' \dontrun{
#' get_clipboard_image()
#' }
#'
#' data(clipboard_sample)
#' head(clipboard_sample, 100)
#' header <- create_header(clipboard_sample)
#' image_data <- c(header, clipboard_sample)
#' path <- fs::path_temp(ext = "bmp")
#' save_bmp(image_data, path)
#'  # shell.exec(path)
#'
#' @export
get_clipboard_image <- function(){
  readClipboard(format = 8, raw = TRUE)
}

#' Create a BMP header
#'
#' @param clipboard A raw vector of the clipboard contents.
#'
#' @return A raw vector of the BMP header.
#'
#' @examples
#' data(clipboard_sample)
#' create_header(clipboard_sample)
#'
#' @export
create_header <- function(clipboard){
  header <-
    c("42","4d",
      "00","00","00","00", # [3:6] temporary: 00
      "00","00",
      "00","00",
        # 42 (hex), 66 (dec): 14 (file header) + 40 (info header) + 12 (palette)
      "42","00","00","00")
  len_clipboard <- length(clipboard) # size of clipboard
  len_header <- length(header) # size of header
  len_file <- len_clipboard + len_header
  header[3:6] <-
    as.hexmode(len_file) |>
    hex2little_endian()
  header <-
    header |>
    as.hexmode() |>
    as.raw()
  return(header)
}
