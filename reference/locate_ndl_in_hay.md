# Locate needle image matrix position in a haystack_image matrix. Helper function for `locate_image()`.

Searches the value that the needle and the haystack have in common and
that appears fewest times in the haystack, and uses one of its positions
as an anchor. Every candidate position of the needle is then a
difference between a position of the value in the haystack and the
anchor, so only those candidates have to be compared.

## Usage

``` r
locate_ndl_in_hay(ndl_mt, hay_mt, exact = TRUE, timeout = 5, tol = 0)
```

## Arguments

- ndl_mt, hay_mt:

  A matrix

- exact:

  A logical. Check matching exactly or not. FALSE compares sampled
  pixels only.

- timeout:

  A numeric for timeout seconds.

- tol:

  A numeric for the tolerance of the comparison, in steps of 255
  grayscale levels. 0 needs an exact match. Use a positive value for an
  image that has been through a lossy format such as JPEG.

## Value

        A numeric pair of xy location for needle image.

## Examples

``` r
haystack_image <- imager::load.example("parrots")
w <- 100
h <- 50
needle_image <- crop_image(haystack_image, 129, 257, w, h)
hay_mt <- image2gray_matrix(haystack_image)
ndl_mt <- image2gray_matrix(needle_image)
(pos <- locate_ndl_in_hay(ndl_mt, hay_mt))
#> [1] 129 257

found <- crop_image(haystack_image, pos[1], pos[2], w, h)
layout(c(1:3))
plot(haystack_image)
plot(needle_image)
plot(found)

```
