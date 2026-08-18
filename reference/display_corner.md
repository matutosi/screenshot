# Get display corner of screen

This function returns the coordinates of the specified corner of the
display. This function works only on windows.

## Usage

``` r
display_corner(size, corner = "bottom_left", width = 600, height = 600)
```

## Arguments

- size:

  Integers to specify width or height of display size.

- corner:

  A string to specify a corner of the display. "top_left", "top_right",
  "bottom_left", or "bottom_right".

- width, height:

  A integer to specify width or height of the corner.

## Value

A numeric vector of length 4 representing the coordinates of the
specified corner.

## Examples

``` r
if (FALSE) { # \dontrun{
size <- display_size()
display_corner(size, "top_left", 800, 800)
} # }
```
