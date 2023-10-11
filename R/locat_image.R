#' Locate needle image position on a screenshot image.
#' 
#' @param needle_image  A string of image file path or 
#'                      a cimg class object of imager library.
#' @param center        A logical. TRUE returns center position of needle_image.
#' @param exact         A logical. Check matching exactly or not.
#' @param timeout       A numeric for timeout seconds.
#' @param corner        A string to specify a corner of the display. 
#'                      "top_left", "top_right", "bottom_left", or "bottom_right".
#' @param width,height  A integer to specify width or height of the corner.
#' @param bin_dir       A string for directory name of screenshot.exe on Win.
#' @return        A numeric pair of xy location.
#' @examples
#' if(interactive()){
#' 
#' sc <- screenshot()
#' if(sc != ""){
#'   sc_image <- imager::load.image(sc)
#'   w <- 100
#'   h <- 80
#'   pos_x <- 1
#'   pos_y <- imager::height(sc_image) - h
#'   needle <- hay2needle(sc_image, pos_x, pos_y, w, h)
#'   (locate_image(needle)) # center location
#'   pos <- locate_image(needle, center = FALSE)
#'   found <- hay2needle(sc_image, pos[1], pos[2], w, h)
#'   layout(c(1:3))
#'   plot(sc_image)
#'   plot(needle)
#'   plot(found)
#'   # usse `coner` argument to limit searching field
#'   pos <- locate_image(needle, corner = "bottom_left", center = FALSE)
#' }
#' 
#' }
#' 
#' @export
locate_image <- function(needle_image, 
                          center = TRUE, exact = TRUE, timeout = 5,
                          corner = NULL, width = 600, height = 300,
                          bin_dir = ""){
  if(is.character(needle_image)){
    needle_image <- imager::load.image(needle_image)
  }
  if(! "cimg" %in% class(needle_image)){
    stop("needle_image should be cimg class object or image file path")
  }
  sc <- screenshot(bin_dir = bin_dir)
  if(sc == ""){
    message("Could NOT take a screenshot")
    return(c(0,0))
  }
  haystack_image <- imager::load.image(sc)
  scale <- 
    dim(haystack_image)[1] / display_size()$width %>%
  round(2)
  if(!is.null(corner)){
    corner <- display_corner(corner, width, height) * scale
      haystack_image <- hay2needle(haystack_image, 
                                 corner[1], corner[2], corner[3], corner[4])
  }else{
    corner <- c(0,0,0,0)
  }
  ndl_mt <- image2gray_matrix(needle_image)
  hay_mt <- image2gray_matrix(haystack_image)
  pos <- (locate_ndl_in_hay(ndl_mt, hay_mt, exact, timeout) + corner[1:2]) / scale
  if(center){
    return(c(pos[1] + imager::width(needle_image)/2  %>% floor(),
             pos[2] + imager::height(needle_image)/2 %>% floor() ))
  }
  return(pos)
}

#' Convert cimg class into grayscale xy matrix.
#' Helper function for `locate_image()`.
#' Use grayscale to Speed up and to simplify code.
#' 
#' @param img   A cimg object.
#' @return      An xy dimensional matrix.
#' 
#' @export
image2gray_matrix <- function(img){
  img <- 
    img %>%
    imager::rm.alpha() %>%
    imager::grayscale()
  return(img[,,1,1])
}

#' Locate needle image  matrix position in a haystack_image matrix.
#' Helper function for `locate_image()`.
#' 
#' @param ndl_mt,hay_mt  A matrix
#' @param timeout        A numeric for timeout seconds.
#' @param exact          A logical. Check matching exactly or not.
#' @return         A numeric pair of xy location for needle image.
#' @examples
#' haystack_image <- imager::load.example("parrots")
#' w <- 100
#' h <- 50
#' needle_image <- hay2needle(haystack_image, 129, 257, w, h)
#' hay_mt <- image2gray_matrix(haystack_image)
#' ndl_mt <- image2gray_matrix(needle_image)
#' (pos <- locate_ndl_in_hay(ndl_mt, hay_mt))
#' 
#' found <- hay2needle(haystack_image, pos[1], pos[2], w, h)
#' layout(c(1:3))
#' plot(haystack_image)
#' plot(needle_image)
#' plot(found)
#' 
#' @export
locate_ndl_in_hay <- function(ndl_mt, hay_mt, 
                              exact = TRUE, timeout = 5){
  st <- Sys.time()
  comp_table <- compare_table(ndl_mt, hay_mt)
  val <- comp_table$val
  # first match
  pos_in_ndl <- xy_pos(ndl_mt, val[1])
  pos_in_hay <- xy_pos(hay_mt, val[1])
  base_xy <- purrr::map(pos_in_hay, `-`, pos_in_ndl[[1]])
  if(length(base_xy) == 1){
    if(exact){
      if(is_all_same(ndl_mt, hay_mt, base_xy)){
        return(base_xy[[1]] + 1)
      }
      message("Could not find needle_image exactly.")
    }
    return(base_xy[[1]] + 1) # return as a reference
  }
  # second and latter
  for(v in val){
    pos_in_ndl <- xy_pos(ndl_mt, v)
    pos_in_hay <- xy_pos(hay_mt, v)
    for(i in seq_along(pos_in_ndl)){
      base_xy_next <- purrr::map(pos_in_hay, `-`, pos_in_ndl[[i]])
      base_xy <- intersect(base_xy, base_xy_next)
      if(length(base_xy) == 1){
        if(exact){
          if(is_all_same(ndl_mt, hay_mt, base_xy)){
            return(base_xy[[1]] + 1)
          }
          message("Could not find needle_image exactly.")
        }
        return(base_xy[[1]] + 1) # return as a reference
      }
      if(as.numeric(Sys.time() - st) > timeout){
        stop("Could not found needle_image in ", timeout, " seconds")
      }
    }
  }
  message("needle_image Not found in haystack_image")
  return(c(0, 0))
}

#' Helper function for `locate_ndl_in_hay()`.
#' @param ndl_mt,hay_mt  A matrix
#' @param base_xy        A numeric pair of xy location.
#' @return         A logical.
is_all_same <- function(ndl_mt, hay_mt, base_xy){
  rows <- (base_xy[[1]][1] + 1):(base_xy[[1]][1] + nrow(ndl_mt))
  cols <- (base_xy[[1]][2] + 1):(base_xy[[1]][2] + ncol(ndl_mt))
  diff <- sum(ndl_mt != hay_mt[rows, cols])
  if(diff == 0){
    return(TRUE)
  }
  return(FALSE)
}

#' Convert array index into xy location in matrix.
#' Helper function for `locate_ndl_in_hay()`.
#' 
#' @param index,nrow  A numeric.
#' @return            A numeric pair of xy location.
#' @examples
#' nrow <- 4
#' matrix(1:12, nrow = nrow)
#' purrr::map(1:12, index2xy, nrow = nrow)
#' 
#' @export
index2xy <- function(index, nrow){
  x <- index %% nrow
  y <- index %/% nrow
  x[x == 0] <- nrow
  y[x != 0] <- y + 1
  return(c(x, y))
}

#' Get xy position of a value in a matrix
#' Helper function for `locate_ndl_in_hay()`.
#' 
#' @param mt   A matrix
#' @param val  A matrix
#' @return     A numeric pairs of xy location.
#' @examples
#' nrow <- 4
#' mt <- matrix(1:12, nrow = nrow)
#' xy_pos(mt, 5)
#' 
#' @export
xy_pos <- function(mt, val){
  which(mt == val) %>%
    purrr::map(index2xy, nrow(mt))
}

#' Compare values within tow arrays or matrices.
#' Helper function for `locate_ndl_in_hay()`.
#' 
#' @param ndl_mt,hay_mt  A matrix.
#' @return A tibble.
#' @examples
#' val <- seq(from = 0, to = 1, by = 0.1)
#' mt_1 <- matrix(sample(val,  20, replace = TRUE))
#' mt_2 <- matrix(sample(val, 100, replace = TRUE))
#' compare_table(mt_1, mt_2)
#' 
#' @export
compare_table <- function(ndl_mt, hay_mt){
  ndl <- count_val_freq(ndl_mt, "ndl")
  hay <- count_val_freq(hay_mt, "hay")
  dplyr::left_join(ndl, hay) %>%
    dplyr::arrange(hay, ndl)
}

#' Helper function for `compare_table()`.
#' 
#' @param mt       A numeric matrix or array.
#' @param colname  A string of name for count.
#' @return         A dataframe.
#' @examples
#' mt <- sample(1:10, 30, replace = TRUE)
#' count_val_freq(mt, "freq")
#' 
#' @export
count_val_freq <- function(mt, colname){
  val <- "val"
  tibble::tibble({{val}} := as.numeric(mt)) %>%
    dplyr::group_by(.data[[val]]) %>%
    dplyr::summarise({{colname}} := dplyr::n())
}

#' Cut off a part of image from a whole image. 
#' 
#' @param haystack_image An image of cimg.
#' @param pos_x,pos_y    A numeric to indicate the top left corner of cutting image.
#'                       When NULL, position will be randomly sampled.
#' @param w,h            A numeric for width or height of the cutting image.
#' @return               An image of cimg object.
#' @examples
#' haystack_image <- imager::load.example("parrots")
#' needle_image <- hay2needle(haystack_image, 200, 250, 100, 50)
#' layout(c(1:2))
#' plot(haystack_image)
#' plot(needle_image)
#' 
#' @export
hay2needle <- function(haystack_image, pos_x, pos_y, w = 50, h = 20){
  dims <- dim(haystack_image)
  img <- haystack_image[
           pos_x:(pos_x + w - 1), 
           pos_y:(pos_y + h - 1),,]
  dim(img) <- c(w,h,dims[3],dims[4])
  return(imager::cimg(img))
}
