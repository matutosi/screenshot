# Get xy position of a value in a matrix Helper function for `locate_ndl_in_hay()`.

Get xy position of a value in a matrix Helper function for
[`locate_ndl_in_hay()`](https://matutosi.github.io/screenshot/reference/locate_ndl_in_hay.md).

## Usage

``` r
xy_pos(mt, val)
```

## Arguments

- mt:

  A matrix

- val:

  A matrix

## Value

    A numeric pairs of xy location.

## Examples

``` r
nrow <- 4
mt <- matrix(1:12, nrow = nrow)
xy_pos(mt, 5)
#> [[1]]
#> [1] 1 2
#> 
```
