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
#' @param datatableproxy A object with a custom class. That class should implement the generics
#' required for a data.table proxy
#'
#' @return A `datatableproxy` object with a `data.table` interface
#' @export
#' @md
data_table_proxy <- function(datatableproxy) {

  # get column names from proxy object
  proxy_colnames <- fp_colnames(datatableproxy)

  # maximum number of autocomplete columns
  nr_of_display_cols <- min(length(proxy_colnames), 50)

  dt <- data.table::as.data.table(matrix(rep(0, 2 + nr_of_display_cols), nrow = 1))
  colnames(dt) <- c(".proxyobject", ".proxystate", proxy_colnames[1:nr_of_display_cols])

  proxystate <- list(
    colnames = proxy_colnames
  )

  # store proxy object and state data
  dt[, c(".proxyobject", ".proxystate") := list(list(datatableproxy), list(proxystate))]

  # class attribute
  class(dt) <- c("datatableproxy", "data.table", "data.frame")

  dt
}


.get_proxy <- function(x) {
  datatableproxy <- .subset2(x, ".proxyobject")[[1]]
}

.get_proxystate <- function(x) {
  datatableproxy <- .subset2(x, ".proxystate")[[1]]
}


#' @export
`[.datatableproxy` <- function(x, i, j, by, keyby, with, nomatch, mult, roll, rollends,
  which, .SDcols, verbose = FALSE, allow.cartesian, drop, on) {

  if (!missing(on) | !missing(drop) | !missing(allow.cartesian) | !missing(.SDcols) |
      !missing(which) | !missing(rollends) | !missing(roll) | !missing(mult) |
      !missing(nomatch) | !missing(by) | !missing(keyby) | !missing(with)) {
    stop("At this point only i and j arguments are implemented")
  }

  datatableproxy <- .get_datatableproxy(x)

  if (verbose) print(paste("number of arguments to []:", nargs()))

  # Scenario 1: i and j are missing
  # In this case we just copy the proxy object and return a new fsttable object
  if (missing(i) && missing(j)) {

    if (verbose) print("i and j missing")

    return(.data_table_proxy(datatableproxy))
  }

  # Scenario 2: j is missing but not i
  # In this case the user selects rows by specifying an expression or
  # a integer vector with row numbers
  # The datatableproxy object will be updated with a row selection and returned
  if (missing(j)) {

    if (verbose) print("only i selected")

    if (is.integer(i)) {

      if (verbose) print("param i is an integer column")

      # slice rows
      datatableproxy <- fp_slice(datatableproxy, i)

      return(.data_table_proxy(datatableproxy))
    }

    return(.data_table_proxy(datatableproxy))
  }


  # Scenario 3: i is missing but not j
  # In this case the user selects or creates new columns
  # The datatableproxy object will be updated with a column selection and returned
  if (missing(i)) {

    if (verbose) print("only j selected")

    stop("[, j] not implemented yet")
  }

  if (verbose) print("i and j selected")

  stop("[i, j] not implemented yet")
}
