# Saves an image from the clipboard to a file

This function works only on windows.

## Usage

``` r
save_clipboard_image(path = "", reset_transparent = TRUE)
```

## Arguments

- path:

  Optional path to save the image to. If not specified, a temporary file
  will be created.

- reset_transparent:

  A logical. If true, the image of transparent color will be removed.

## Value

The path to the saved image file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Save the image from the clipboard to a file
save_clipboard_image("clipboard_image.png")
} # }
```
