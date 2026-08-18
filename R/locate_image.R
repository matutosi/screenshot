#' Locate needle image position on a screenshot image.
#' 
#' @param needle_image  A string of image file path or 
#'                      a cimg class object of imager library.
#' @param center        A logical. TRUE returns center position of needle_image.
#' @param exact         A logical. Check matching exactly or not.
#'                      FALSE compares sampled pixels only.
#' @param timeout       A numeric for timeout seconds.
#' @param tol           A numeric for the tolerance of the comparison,
#'                      in steps of 255 grayscale levels. 0 needs an exact
#'                      match, which is right for a screenshot saved as PNG.
#'                      Use a positive value for an image that has been
#'                      through a lossy format such as JPEG.
#' @param corner        A string to specify a corner of the display. 
#'                      "top_left", "top_right", "bottom_left", or "bottom_right".
#' @param width,height  A integer to specify width or height of the corner.
#' @param size          Integers to specify width or height of display size.
#' @param scale         A numeric to specify display scale.
#' @param bin_dir       A string for directory name of screenshot.exe on Win.
#' @return        A numeric pair of xy location.
#' @examples
#' \dontrun{
#' sc <- screenshot()
#' if(sc != ""){
#'   sc_image <- imager::load.image(sc)
#'   w <- 100
#'   h <- 80
#'   pos_x <- 1
#'   pos_y <- imager::height(sc_image) - h
#'   needle <- crop_image(sc_image, pos_x, pos_y, w, h)
#'   (locate_image(needle)) # center location
#'   pos <- locate_image(needle, center = FALSE)
#'   found <- crop_image(sc_image, pos[1], pos[2], w, h)
#'   layout(c(1:3))
#'   plot(sc_image)
#'   plot(needle)
#'   plot(found)
#'   # usse `coner` to limit searching field
#'   # `coner` can be used in Windows
#'   pos <- locate_image(needle, corner = "bottom_left", center = FALSE)
#' }
#' }
#' 
#' @export
locate_image <- function(needle_image, 
                         center = TRUE, exact = TRUE, timeout = 5,
                         corner = NULL, width = 600, height = 300,
                         size = NULL, scale = NULL,
                         bin_dir = "", tol = 0){
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
  if(is.null(scale)){
    scale <- 
      round(dim(haystack_image)[1] / display_size()$width, 2)
  }
  if(is.null(size)){
    size <- display_size()
  }
  if(!is.null(corner)){
    corner <- display_corner(size = size, corner, width, height) * scale
    haystack_image <- crop_image(haystack_image, 
                                 corner[1], corner[2], corner[3], corner[4])
  }else{
    corner <- c(0,0,0,0)
  }
  ndl_mt <- image2gray_matrix(needle_image)
  hay_mt <- image2gray_matrix(haystack_image)
  pos <- (locate_ndl_in_hay(ndl_mt, hay_mt, exact, timeout, tol) + corner[1:2]) / scale
  if(center){
    return(c(pos[1] + floor(imager::width(needle_image)  / 2),
             pos[2] + floor(imager::height(needle_image) / 2)))
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
    img |>
    imager::rm.alpha() |>
    imager::grayscale()
  return(img[,,1,1])
}

#' Locate needle image  matrix position in a haystack_image matrix.
#' Helper function for `locate_image()`.
#' 
#' Searches the value that the needle and the haystack have in common and
#' that appears fewest times in the haystack, and uses one of its positions
#' as an anchor. Every candidate position of the needle is then a difference
#' between a position of the value in the haystack and the anchor, so only
#' those candidates have to be compared.
#' 
#' @param ndl_mt,hay_mt  A matrix
#' @param timeout        A numeric for timeout seconds.
#' @param exact          A logical. Check matching exactly or not.
#'                       FALSE compares sampled pixels only.
#' @param tol            A numeric for the tolerance of the comparison,
#'                       in steps of 255 grayscale levels. 0 needs an exact
#'                       match. Use a positive value for an image that has
#'                       been through a lossy format such as JPEG.
#' @return         A numeric pair of xy location for needle image.
#' @examples
#' haystack_image <- imager::load.example("parrots")
#' w <- 100
#' h <- 50
#' needle_image <- crop_image(haystack_image, 129, 257, w, h)
#' hay_mt <- image2gray_matrix(haystack_image)
#' ndl_mt <- image2gray_matrix(needle_image)
#' (pos <- locate_ndl_in_hay(ndl_mt, hay_mt))
#' 
#' found <- crop_image(haystack_image, pos[1], pos[2], w, h)
#' layout(c(1:3))
#' plot(haystack_image)
#' plot(needle_image)
#' plot(found)
#' 
#' @export
locate_ndl_in_hay <- function(ndl_mt, hay_mt, 
                              exact = TRUE, timeout = 5, tol = 0){
  st <- Sys.time()
  n_row <- nrow(ndl_mt)
  n_col <- ncol(ndl_mt)
  if(n_row > nrow(hay_mt) || n_col > ncol(hay_mt)){
    message("needle_image is larger than haystack_image")
    return(c(0, 0))
  }
  tol_val <- tol2value(tol)
  val <- anchor_value(ndl_mt, hay_mt, tol_val)
  if(is.null(val)){
    message("needle_image Not found in haystack_image")
    return(c(0, 0))
  }
  # one position of the anchor value is enough to enumerate the candidates
  anchor <- unname(which(ndl_mt == val, arr.ind = TRUE)[1, ])
  cand <- if(tol_val > 0){
            which(abs(hay_mt - val) <= tol_val, arr.ind = TRUE)
          }else{
            which(hay_mt == val, arr.ind = TRUE)
          }
  cand <- unname(cand) # drop the "row" and "col" names of which(arr.ind = TRUE)
  probe <- probe_points(n_row, n_col)
  probe_val <- ndl_mt[probe]
  for(k in seq_len(nrow(cand))){
    base_xy <- c(cand[k, 1], cand[k, 2]) - anchor
    if(base_xy[1] < 0 || base_xy[2] < 0 ||
       base_xy[1] + n_row > nrow(hay_mt) ||
       base_xy[2] + n_col > ncol(hay_mt)){
      next
    }
    # cheap rejection on a few spread pixels before comparing the whole block
    hay_probe <- hay_mt[cbind(probe[, 1] + base_xy[1], probe[, 2] + base_xy[2])]
    if(any(abs(hay_probe - probe_val) > tol_val)){
      next
    }
    if(!exact || is_all_same(ndl_mt, hay_mt, base_xy, tol_val)){
      return(base_xy + 1)
    }
    if(as.numeric(Sys.time() - st) > timeout){
      stop("Could not found needle_image in ", timeout, " seconds")
    }
  }
  message("needle_image Not found in haystack_image")
  return(c(0, 0))
}

#' Convert a tolerance in grayscale steps into a value to compare with.
#' Helper function for `locate_ndl_in_hay()`.
#' 
#' Half a step is added because a grayscale value is a weighted sum of the
#' RGB channels, so a difference of one step is not exactly 1/255 and would
#' fall on the wrong side of the comparison.
#' 
#' @param tol  A numeric in steps of 255 grayscale levels.
#' @return     A numeric.
#' @noRd
tol2value <- function(tol){
  if(is.null(tol) || is.na(tol) || tol <= 0){
    return(0)
  }
  return((tol + 0.5) / 255)
}

#' Pixels spread over the needle image, used to reject a candidate cheaply.
#' Helper function for `locate_ndl_in_hay()`.
#' 
#' @param n_row,n_col  An integer of the needle size.
#' @param n            An integer of the number of pixels per side.
#' @return             A two column matrix of row and column.
#' @noRd
probe_points <- function(n_row, n_col, n = 4L){
  rows <- unique(round(seq(1, n_row, length.out = min(n, n_row))))
  cols <- unique(round(seq(1, n_col, length.out = min(n, n_col))))
  return(as.matrix(expand.grid(row = rows, col = cols)))
}

#' Find the value to use as an anchor of the search.
#' Helper function for `locate_ndl_in_hay()`.
#' 
#' Returns the value of the needle that gives the fewest candidates in the
#' haystack. Only the values of the needle are counted, which is far cheaper
#' than a frequency table of the whole haystack.
#' 
#' @param ndl_mt,hay_mt  A matrix.
#' @param tol_val        A numeric of the tolerance of the comparison.
#' @return               A numeric, or NULL when no value is shared.
#' @noRd
anchor_value <- function(ndl_mt, hay_mt, tol_val = 0){
  ndl <- as.numeric(ndl_mt)
  hay <- as.numeric(hay_mt)
  if(tol_val <= 0){
    val <- unique(ndl)
    cnt <- tabulate(match(hay, val), nbins = length(val))
    cnt[cnt == 0] <- NA # a value missing from the haystack is not a candidate
    if(all(is.na(cnt))){
      return(NULL)
    }
    return(val[which.min(cnt)])
  }
  # with a tolerance, count on 255 grayscale levels and sum over the window
  lv_ndl <- val2level(ndl)
  lv_hay <- val2level(hay)
  tol_lv <- max(round(tol_val * 255), 1)
  cum <- c(0, cumsum(tabulate(lv_hay + 1L, nbins = 256L)))
  lv <- unique(lv_ndl)
  cnt <- cum[pmin(lv + tol_lv, 255L) + 2L] - cum[pmax(lv - tol_lv, 0L) + 1L]
  cnt[cnt == 0] <- NA
  if(all(is.na(cnt))){
    return(NULL)
  }
  return(ndl[which(lv_ndl == lv[which.min(cnt)])[1]])
}

#' Convert a grayscale value into one of 255 levels.
#' Helper function for `anchor_value()`.
#' 
#' @param val  A numeric.
#' @return     An integer within 0 and 255.
#' @noRd
val2level <- function(val){
  return(as.integer(pmin(pmax(round(val * 255), 0), 255)))
}

#' Helper function for `locate_ndl_in_hay()`.
#' @param ndl_mt,hay_mt  A matrix
#' @param base_xy        A numeric pair of xy location.
#' @param tol_val        A numeric of the tolerance of the comparison.
#' @return         A logical.
is_all_same <- function(ndl_mt, hay_mt, base_xy, tol_val = 0){
  rows <- base_xy[1] + seq_len(nrow(ndl_mt))
  cols <- base_xy[2] + seq_len(ncol(ndl_mt))
  if(min(rows) < 1 || min(cols) < 1 ||
     max(rows) > nrow(hay_mt) || max(cols) > ncol(hay_mt)){
    return(FALSE)
  }
  return(all(abs(hay_mt[rows, cols] - ndl_mt) <= tol_val))
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
  x <- (index - 1) %%  nrow + 1
  y <- (index - 1) %/% nrow + 1
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
  which(mt == val) |>
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
  dplyr::left_join(ndl, hay, by = "val") |>
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
  tibble::tibble({{ val }} := as.numeric(mt)) |>
    dplyr::group_by(dplyr::pick({{ val }})) |>
    dplyr::summarise({{ colname }} := dplyr::n())
}


#' Cut off a part of image from a whole image. 
#' 
#' @name crop_image
#' @param image An image of cimg.
#' @param pos_x,pos_y    A numeric to indicate the top left corner of cutting image.
#'                       When NULL, position will be randomly sampled.
#' @param w,h            A numeric for width or height of the cutting image.
#' @return               An image of cimg object.
#' @examples
#' image <- imager::load.example("parrots")
#' croped_image <- crop_image(image, 200, 250, 100, 50)
#' layout(c(1:2))
#' plot(image)
#' plot(croped_image)
#' 
#' @export
crop_image <- function(image, pos_x, pos_y, w = 50, h = 20){
  dims <- dim(image)
  if(pos_x < 1 || pos_y < 1 ||
     pos_x + w - 1 > dims[1] || pos_y + h - 1 > dims[2]){
    stop("Cutting area (", pos_x, ", ", pos_y, ", ", w, ", ", h,
         ") is outside of the image (", dims[1], " x ", dims[2], ")")
  }
  img <- image[
           pos_x:(pos_x + w - 1), 
           pos_y:(pos_y + h - 1),,]
  dim(img) <- c(w, h, dims[3], dims[4])
  return(imager::cimg(img))
}

#' @rdname crop_image
#' @export
hay2needle <- function(image, pos_x, pos_y, w = 50, h = 20){
  .Deprecated("crop_image", msg =
    "'hay2needle()' is deprecated and will be removed in version 1.0.0. Use 'crop_image()' instead.")
  crop_image(image, pos_x, pos_y, w = w, h = h)
}
