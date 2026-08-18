# Retrieves the image from the clipboard

This function works only on windows.

## Usage

``` r
get_clipboard_image()
```

## Value

A raw vector containing the image data.

## Examples

``` r
if (FALSE) { # \dontrun{
get_clipboard_image()
} # }

data(clipboard_sample)
head(clipboard_sample, 100)
#>   [1] 28 00 00 00 01 02 00 00 78 00 00 00 01 00 20 00 03 00 00 00 e0 c1 03 00 00
#>  [26] 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ff 00 00 ff 00 00 ff 00
#>  [51] 00 00 00 00 00 ff 00 00 00 ff 00 00 00 ff 00 00 00 ff 00 00 00 ff 00 00 00
#>  [76] ff 00 00 00 ff 00 00 00 ff 00 00 00 ff 00 00 00 ff 00 00 00 ff 00 00 00 ff
header <- create_header(clipboard_sample)
image_data <- c(header, clipboard_sample)
path <- fs::path_temp(ext = "bmp")
save_bmp(image_data, path)
#> /tmp/RtmpvbP5VQ.bmp
 # shell.exec(path)
fs::file_delete(path)
```
