# Convert array index into xy location in matrix. Helper function for `locate_ndl_in_hay()`.

Convert array index into xy location in matrix. Helper function for
[`locate_ndl_in_hay()`](https://matutosi.github.io/screenshot/reference/locate_ndl_in_hay.md).

## Usage

``` r
index2xy(index, nrow)
```

## Arguments

- index, nrow:

  A numeric.

## Value

           A numeric pair of xy location.

## Examples

``` r
nrow <- 4
matrix(1:12, nrow = nrow)
#>      [,1] [,2] [,3]
#> [1,]    1    5    9
#> [2,]    2    6   10
#> [3,]    3    7   11
#> [4,]    4    8   12
purrr::map(1:12, index2xy, nrow = nrow)
#> [[1]]
#> [1] 1 1
#> 
#> [[2]]
#> [1] 2 1
#> 
#> [[3]]
#> [1] 3 1
#> 
#> [[4]]
#> [1] 4 2
#> 
#> [[5]]
#> [1] 1 2
#> 
#> [[6]]
#> [1] 2 2
#> 
#> [[7]]
#> [1] 3 2
#> 
#> [[8]]
#> [1] 4 3
#> 
#> [[9]]
#> [1] 1 3
#> 
#> [[10]]
#> [1] 2 3
#> 
#> [[11]]
#> [1] 3 3
#> 
#> [[12]]
#> [1] 4 4
#> 
```
