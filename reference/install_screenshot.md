# Install command line screenshot for Windows.

Codes are from URL shown below.
https://superuser.com/questions/75614/take-a-screen-shot-from-command-line-in-windows#answer-1751844
On Mac `screencapture` is usually available. On Linux GNOME desktop use
`gnome-screenshot`. If not installed, run
`sudo apt install gnome-screenshot`.

## Usage

``` r
install_screenshot(bin_dir = "")
```

## Arguments

- bin_dir:

  A string of directory to be installed.

## Value

       A string of installed directory.

## Examples

``` r
if(interactive()){

# need only on Win
if(get_os() == "win"){
  bin_dir <- fs::path_package("screenshot")
  # if you want to install another directory
  #   bin_dir <- "SET_YOUR DIRECTORY"
  install_screenshot(bin_dir)
}

}
```
