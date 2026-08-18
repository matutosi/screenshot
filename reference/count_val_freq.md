# Helper function for `compare_table()`.

Helper function for
[`compare_table()`](https://matutosi.github.io/screenshot/reference/compare_table.md).

## Usage

``` r
count_val_freq(mt, colname)
```

## Arguments

- mt:

  A numeric matrix or array.

- colname:

  A string of name for count.

## Value

        A dataframe.

## Examples

``` r
mt <- sample(1:10, 30, replace = TRUE)
count_val_freq(mt, "freq")
#> # A tibble: 10 × 2
#>      val  freq
#>    <dbl> <int>
#>  1     1     3
#>  2     2     3
#>  3     3     3
#>  4     4     4
#>  5     5     3
#>  6     6     4
#>  7     7     2
#>  8     8     2
#>  9     9     2
#> 10    10     4
```
