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


#' Access a fst file like a regular data frame
#'
#' Create a fsttable object that can be accessed like a regular data frame. This object
#' is just a reference to the actual data and requires only a small amount of memory.
#' When data is accessed, only the requested subset is read from file. This is possible
#' because the fst file format allows full random access (in columns and rows) to the stored
#' dataset.
#'
#' @param path Path to a fst file
#' @param old_format Use \code{old_format = TRUE} when referencing a fst file that was created
#' with a fst package version lower than 0.8.0
#'
#' @return An object of class \code{fsttable}
#' @export
#' @examples
#' \dontrun{
#' # generate a sample fst file
#' path <- paste0(tempfile(), ".fst")
#' write_fst(iris, path)
#'
#' # create a fsttable object that can be used as a data frame
#' ft <- fst(path)
#'
#' # print head and tail
#' print(ft)
#'
#' # select columns and rows
#' x <- ft[10:14, c("Petal.Width", "Species")]
#'
#' # use the common list interface
#' ft[TRUE]
#' ft[c(TRUE, FALSE)]
#' ft[["Sepal.Length"]]
#' ft$Petal.Length
#'
#' # use data frame generics
#' nrow(ft)
#' ncol(ft)
#' dim(ft)
#' dimnames(ft)
#' colnames(ft)
#' rownames(ft)
#' names(ft)
#' }
fst_table <- function(path, old_format = FALSE) {

  # The fsttable object is wrapped in a data.table to keep code completion working in RStudio
  # Each column corresponds to an actual column in the underlying fst file

  fstproxy <- fst_proxy(path, old_format)

  .fst_table(fstproxy)
}


.fst_table <- function(fstproxy) {
  dt <- data.table::as.data.table(matrix(rep(0, 1 + fp_ncol(fstproxy)), nrow = 1))
  colnames(dt) <- c(".FstData", fp_colnames(fstproxy))
  
  fsttable_data <- list(
    fstproxy = fstproxy
  )
  
  dt[, .FstData := list(list(fsttable_data))]
  
  # class attribute
  class(dt) <- c("fsttable", "data.table", "data.frame")
  
  dt
}


.column_indexes_fst <- function(fstproxy, j) {

  # test correct column names
  if (is.character(j) ) {
    wrong <- !(j %in% fp_colnames(fstproxy))

    if (any(wrong)) {
      names <- j[wrong]
      stop(sprintf("Undefined columns: %s", paste(names, collapse = ", ")))
    }
  } else if (is.logical(j)) {

    if (any(is.na(j))) {
      stop("Subscript out of bounds.", call. = FALSE)
    }

    j <- fp_colnames(fstproxy)[j]
  } else if (is.numeric(j)) {
    if (any(is.na(j))) {
      stop("Subscript out of bounds.", call. = FALSE)
    }

    if (any(j <= 0)) {
      stop("Only positive column indexes supported.", call. = FALSE)
    }

    if (any(j > fp_ncol(fstproxy))) {
      stop("Subscript out of bounds.", call. = FALSE)
    }

    j <- fp_colnames(fstproxy)[j]
  }

  j
}


.get_fstproxy <- function(x) {
  fstproxy <- .subset2(x, ".FstData")[[1]][["fstproxy"]]
}


#' @export
`[.fsttable` <- function(x, i, j, by, keyby, with, nomatch, mult, roll, rollends,
  which, .SDcols, verbose = FALSE, allow.cartesian, drop, on) {

  if (!missing(on) | !missing(drop) | !missing(allow.cartesian) | !missing(.SDcols) |
      !missing(which) | !missing(rollends) | !missing(roll) | !missing(mult) |
      !missing(nomatch) | !missing(by) | !missing(keyby) | !missing(with)) {
    stop("At this point only i and j arguments are implemented")
  }
    
  fstproxy <- .get_fstproxy(x)

  if (verbose) print(paste("number of arguments to []:", nargs()))
  
  # Scenario 1: i and j are missing
  # In this case we just copy the proxy object and return a new fsttable object
  if (missing(i) && missing(j)) {

    if (verbose) print("i and j missing")
    
    return(.fst_table(fstproxy))
  }

  # Scenario 2: j is missing but not i
  # In this case the user selects rows by specifying an expression or
  # a integer vector with row numbers 
  # The fstproxy object will be updated with a row selection and returned
  if (missing(j)) {

    if (verbose) print("only i selected")
    
    if (is.integer(i)) {
      
      if (verbose) print("param i is an integer column")
      
      # slice rows
      fstproxy <- fp_slice(fstproxy, i)
      
      return(.fst_table(fstproxy))
    }
    
    return(.fst_table(fstproxy))
  }
  

  # Scenario 3: i is missing but not j
  # In this case the user selects or creates new columns
  # The fstproxy object will be updated with a column selection and returned
  if (missing(i)) {
    
    if (verbose) print("only j selected")

    stop("[, j] not implemented yet")
  }

  if (verbose) print("i and j selected")
  
  stop("[i, j] not implemented yet")
  
  
  # if (nargs() <= 2) {
  # 
  #   # return full table
  #   if (missing(i)) {
  #     # we have a j
  #     j <- .column_indexes_fst(meta_info, j)
  #     return(read_fst(meta_info$path, j, old_format = .get_FstData(x, "old_format")))
  #   }
  # 
  #   # i is interpreted as j
  #   j <- .column_indexes_fst(meta_info, i)
  #   return(read_fst(meta_info$path, j, old_format = .get_FstData(x, "old_format")))
  # }
  # 
  # # return all rows
  # 
  # # special case where i is interpreted as j: select all rows
  # if (nargs() == 3 && !missing(drop) && !missing(i)) {
  #   j <- .column_indexes_fst(meta_info, i)
  #   return(read_fst(meta_info$path, j, old_format = .get_FstData(x, "old_format")))
  # }
  # 
  # # i and j not reversed
  # 
  # # full columns
  # if (missing(i)) {
  #   j <- .column_indexes_fst(meta_info, j)
  #   return(read_fst(meta_info$path, j, old_format = .get_FstData(x, "old_format")))
  # }
  # 
  # 
  # # determine integer vector from i
  # if (!(is.numeric(i) || is.logical(i))) {
  #   stop("Row selection should be done using a numeric or logical vector", call. = FALSE)
  # }
  # 
  # if (is.logical(i)) {
  #   i <- which(i)
  # }
  # 
  # # cast to integer and determine row range
  # i <- as.integer(i)
  # min_row <- min(i)
  # max_row <- max(i)
  # 
  # # boundary check
  # if (min_row < 0) {
  #   stop("Row selection out of range")
  # }
  # 
  # if (max_row > meta_info$nrOfRows) {
  #   stop("Row selection out of range")
  # }
  # 
  # # column subset
  # 
  # # select all columns
  # if (missing(j)) {
  #   fst_data <- read_fst(meta_info$path, from = min_row, to = max_row, old_format = .get_FstData(x, "old_format"))
  # 
  #   return(fst_data[1 + i - min_row, ])
  # }
  # 
  # j <- .column_indexes_fst(meta_info, j)
  # fst_data <- read_fst(meta_info$path, j, from = min_row, to = max_row, old_format = .get_FstData(x, "old_format"))
  # 
  # fst_data[1 + i - min_row, ]
}
