# Create a BMP header

Create a BMP header

## Usage

``` r
create_header(clipboard)
```

## Arguments

- clipboard:

  A raw vector of the clipboard contents.

## Value

A raw vector of the BMP header.

## Examples

``` r
data(clipboard_sample)
create_header(clipboard_sample)
#>  [1] 42 4d 22 c2 03 00 00 00 00 00 42 00 00 00
```
