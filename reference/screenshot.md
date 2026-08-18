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

  A logical. If true, quote the screenshot command.

## Value

        A file name of screenshot. When "", screenshot will be saved in a tempral directory.

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
