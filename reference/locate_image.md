# Locate needle image position on a screenshot image.

Locate needle image position on a screenshot image.

## Usage

``` r
locate_image(
  needle_image,
  center = TRUE,
  exact = TRUE,
  timeout = 5,
  corner = NULL,
  width = 600,
  height = 300,
  size = NULL,
  scale = NULL,
  bin_dir = "",
  tol = 0
)
```

## Arguments

- needle_image:

  A string of image file path or a cimg class object of imager library.

- center:

  A logical. TRUE returns center position of needle_image.

- exact:

  A logical. Check matching exactly or not. FALSE compares sampled
  pixels only.

- timeout:

  A numeric for timeout seconds.

- corner:

  A string to specify a corner of the display. "top_left", "top_right",
  "bottom_left", or "bottom_right".

- width, height:

  A integer to specify width or height of the corner.

- size:

  Integers to specify width or height of display size.

- scale:

  A numeric to specify display scale.

- bin_dir:

  A string for directory name of screenshot.exe on Win.

- tol:

  A numeric for the tolerance of the comparison, in steps of 255
  grayscale levels. 0 needs an exact match, which is right for a
  screenshot saved as PNG. Use a positive value for an image that has
  been through a lossy format such as JPEG.

## Value

       A numeric pair of xy location.

## Examples

``` r
if (FALSE) { # \dontrun{
sc <- screenshot()
if(sc != ""){
  sc_image <- imager::load.image(sc)
  w <- 100
  h <- 80
  pos_x <- 1
  pos_y <- imager::height(sc_image) - h
  needle <- crop_image(sc_image, pos_x, pos_y, w, h)
  (locate_image(needle)) # center location
  pos <- locate_image(needle, center = FALSE)
  found <- crop_image(sc_image, pos[1], pos[2], w, h)
  layout(c(1:3))
  plot(sc_image)
  plot(needle)
  plot(found)
  # usse `coner` to limit searching field
  # `coner` can be used in Windows
  pos <- locate_image(needle, corner = "bottom_left", center = FALSE)
}
} # }
```
