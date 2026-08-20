# Take a screenshot.

Need to install screenshot.exe on Win by install_screenshot().

## Usage

``` r
screenshot(file = "", bin_dir = "", quote = FALSE)
```

## Arguments

- file:

  A string for file name of screenshot.

- bin_dir:

  A string for directory name of screenshot.exe on Win.

- quote:

  Deprecated and ignored. The command is always quoted.

## Value

        A file name of screenshot. When "", screenshot will be saved in a tempral directory.

## Details

The command is always quoted, so that a path with a space (such as
"C:/Program Files/R/R-4.5.1/library/screenshot") works.

## See also

       install_screenshot()

## Examples

``` r
if(interactive()){

sc <- screenshot()
if(sc != ""){
  sc_image <- imager::load.image(sc)
  plot(sc_image)
}

}
```
