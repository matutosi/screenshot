# Compare values within tow arrays or matrices. Helper function for `locate_ndl_in_hay()`.

Compare values within tow arrays or matrices. Helper function for
[`locate_ndl_in_hay()`](https://matutosi.github.io/screenshot/reference/locate_ndl_in_hay.md).

## Usage

``` r
compare_table(ndl_mt, hay_mt)
```

## Arguments

- ndl_mt, hay_mt:

  A matrix.

## Value

A tibble.

## Examples

``` r
val <- seq(from = 0, to = 1, by = 0.1)
mt_1 <- matrix(sample(val,  20, replace = TRUE))
mt_2 <- matrix(sample(val, 100, replace = TRUE))
compare_table(mt_1, mt_2)
#> # A tibble: 9 × 3
#>     val   ndl   hay
#>   <dbl> <int> <int>
#> 1   0.7     1     5
#> 2   1       2     5
#> 3   0.3     2     6
#> 4   0.4     5     8
#> 5   0.8     1     9
#> 6   0.2     1    11
#> 7   0.6     2    11
#> 8   0.1     3    11
#> 9   0.5     3    13
```
