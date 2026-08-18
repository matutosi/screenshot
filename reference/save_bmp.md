# Save an image as a BMP file

Save an image as a BMP file

## Usage

``` r
save_bmp(image_data, path)
```

## Arguments

- image_data:

  A raster image data object, such as an array of pixel values or an R
  object representing an image.

- path:

  The path to the file to be saved.

## Value

Saves the image as a BMP file at the specified path.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create an image data object
image_data <- matrix(rnorm(100), ncol = 10)
# Save the image as a BMP file
save_bmp(image_data, "image.bmp")
} # }
```
