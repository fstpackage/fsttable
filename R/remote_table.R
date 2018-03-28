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


rtable_ncol <- function (x, ...) {
   UseMethod("rtable_ncol", x)
}


rtable_nrow <- function (x, ...) {
   UseMethod("rtable_nrow", x)
}


rtable_colnames <- function (x, ...) {
   UseMethod("rtable_colnames", x)
}


rtable_read_range <- function (x, from_row, to_row, colnames = NULL, ...) {
   UseMethod("rtable_read_range", x)
}


rtable_read_full <- function (x, colnames = NULL, ...) {
   UseMethod("rtable_read_full", x)
}


rtable_column_types <- function (x, ...) {
   UseMethod("rtable_column_types", x)
}
