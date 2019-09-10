
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fsttable

<!-- badges: start -->

[![Linux/OSX Build
Status](https://travis-ci.org/fstpackage/fsttable.svg?branch=develop)](https://travis-ci.org/fstpackage/fsttable)
[![Windows Build
status](https://ci.appveyor.com/api/projects/status/nrjyuihxtx9amgpl/branch/develop?svg=true)](https://ci.appveyor.com/project/fstpackage/fsttable)
[![License: AGPL
v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

R package `fsttable` aims to provide a fully functional `data.table`
interface to on-disk `fst` files. The focus of the package is on keeping
memory usage as low as possible woithout sacrificing features of
in-memory `data.table` operations.

## Installation

You can install the latest package version with:

``` r
devtools::install_github("fstpackage/fsttable")
```

## Example

First, we create a on-disk *fst* file containing a medium sized dataset:

``` r
library(fsttable)

# write some sample data to disk
nr_of_rows <- 1e6
x <- data.table::data.table(X = 1:nr_of_rows, Y = LETTERS[1 + (1:nr_of_rows) %% 26])
fst::write_fst(x, "1.fst")
```

Then we define our *fst\_table* by using:

``` r
ft <- fst_table("1.fst")
```

This *fst\_table* can be used as a regular *data.table* object. For
example, we can print:

``` r
ft
#> <fst file>
#> 1e+06 rows, 2 columns
#> 
#>               X     Y
#>           <int> <chr>
#> 1             1     B
#> 2             2     C
#> 3             3     D
#> 4             4     E
#> 5             5     F
#> --           --    --
#> 999996   999996     K
#> 999997   999997     L
#> 999998   999998     M
#> 999999   999999     N
#> 1000000 1000000     O
```

we can select columns:

``` r
ft[, .(Y)]
#> <fst file>
#> 1e+06 rows, 1 columns
#> 
#>             Y
#>         <chr>
#> 1           B
#> 2           C
#> 3           D
#> 4           E
#> 5           F
#> --         --
#> 999996      K
#> 999997      L
#> 999998      M
#> 999999      N
#> 1000000     O
```

and rows:

``` r
ft[1:4,]
#> <fst file>
#> 4 rows, 2 columns
#> 
#>       X     Y
#>   <int> <chr>
#> 1     1     B
#> 2     2     C
#> 3     3     D
#> 4     4     E
```

Or both at the same time:

``` r
ft[1:4, .(X)]
#> <fst file>
#> 4 rows, 1 columns
#> 
#>       X
#>   <int>
#> 1     1
#> 2     2
#> 3     3
#> 4     4
```

# Memory

During the operations shown above, the actual data was never fully
loaded from the file. That’s because of `fsttable`’s philosophy of
keeping RAM usage as low as possible. Printing a few lines of a table
doesn’t require knowlegde of the remaining lines, so `fsttable` will
never actualy load them.

Even when you create a new set:

``` r
ft2 <- ft[1:4, .(X)]
```

No actual data is being loaded into RAM. The copy still uses the
original *fst* file to keep the data on-disk:

``` r
# small size because actual data is still on disk
object.size(ft2)
#> 5808 bytes
```
