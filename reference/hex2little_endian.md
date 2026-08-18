# Convert hexadecimal string to little-endian

Convert hexadecimal string to little-endian

## Usage

``` r
hex2little_endian(x)
```

## Arguments

- x:

  Hexadecimal string

## Value

Little-endian hexadecimal string

## Examples

``` r
hex2little_endian("01234567")
#> [1] "67" "45" "23" "01"
hex2little_endian("012345")
#> [1] "45" "23" "01" "00"
```
