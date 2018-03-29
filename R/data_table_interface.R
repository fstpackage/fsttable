#  fsttable - A 'data.table' interface to on-disk fst files.
#
#  Copyright (C) 2017-present, Mark AJ Klik and Martin Blostein
#
#  This file is part of the fsttable R package.
#
#  The fsttable R package is free software: you can redistribute it and/or modify it
#  under the terms of the GNU Affero General Public License version 3 as
#  published by the Free Software Foundation.
#
#  The fsttable R package is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
#  for more details.
#
#  You should have received a copy of the GNU Affero General Public License along
#  with the fsttable R package. If not, see <http://www.gnu.org/licenses/>.
#
#  You can contact the author at:
#  - fsttable R package source repository : https://github.com/fstpackage/fsttable



#' Create a `data.table` interface around a proxy object
#'
#' This method defines a `data.table` interface around a specific object. That object should
#' define all the generics needed for supporting the interface.
#'
#' @param table_proxy Object generated with method `table_proxy()` that provides an interface
#' to an encapsulated remote table.
#'
#' @return A `datatableinterface` object that exposes a `data.table` interface to the encapsulated
#' `table_proxy` object.
#' @export
#' @md
data_table_interface <- function(table_proxy) {

  # get column names from proxy object
  proxy_colnames <- table_proxy_colnames(table_proxy)

  # maximum number of autocomplete columns
  nr_of_display_cols <- min(length(proxy_colnames), 50)

  dt <- data.table::as.data.table(matrix(rep(0, 1 + nr_of_display_cols), nrow = 1))
  colnames(dt) <- c(".table_proxy", proxy_colnames[1:nr_of_display_cols])

  # store remote proxy object
  dt[, .table_proxy := list(list(table_proxy))]

  # class attribute
  class(dt) <- c("datatableinterface", "data.table", "data.frame")

  dt
}


.get_table_proxy <- function(x) {
  .subset2(x, ".table_proxy")[[1]]
}


#' @export
`[.datatableinterface` <- function(x, i, j, by, keyby, with, nomatch, mult, roll, rollends,
  which, .SDcols, verbose = FALSE, allow.cartesian, drop, on) {

  if (!missing(on) | !missing(drop) | !missing(allow.cartesian) | !missing(.SDcols) |
      !missing(which) | !missing(rollends) | !missing(roll) | !missing(mult) |
      !missing(nomatch) | !missing(by) | !missing(keyby) | !missing(with)) {
    stop("At this point only i and j arguments are implemented")
  }

  table_proxy <- .get_table_proxy(x)

  if (verbose) print(paste("number of arguments to []:", nargs()))

  # Scenario 1: i and j are missing
  # In this case we just copy the proxy object and return a new datatableinterface object
  if (missing(i) && missing(j)) {

    if (verbose) print("i and j missing")

    return(data_table_interface(table_proxy))
  }

  # Row selection is always done before column selection
  # The following holds:
  #
  # ft[1:10, .(ColB)] is equivalent to ft[1:10][, .(ColB)]
  #
  # Therefore we can treat the simulaneous call [i, j] as [i][, j]

  if (!missing(i)) {
    if (verbose) print("i used")

    # return full table, implement later
    return(data_table_interface(table_proxy))
  }

  if (!missing(j)) {
    if (verbose) print("j used")

    jsub <- parse_j(substitute(j))

    print(jsub)

    # parsing j is done in a few steps:
    # 1)
    # 2)

    # return full table, implement later
    return(data_table_interface(table_proxy))
  }
}
