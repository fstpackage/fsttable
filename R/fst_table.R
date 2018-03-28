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


#' Access a fst file like a regular data.table
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

  data_table_proxy(fstproxy)
}
