#' Data Input
#'
#' In addition to \code{read.csv} functionality, this function checks whether
#' the data file satisfies the EPI rules:
#' 1) The first three columns must be 'code', 'iso' and 'country' following
#'    the Masterfile format.
#' 2) All following columns must follow the convention 'NAME.YYYY' where NAME is
#'    the variable name and YYYY the year.
#' 3) NAME must be consistent and same with what is in the file name.
#'
#' @usage read.csv.epi(file, root = NA, year = 2016, allow.na = TRUE)
#'
#' @param file file that contains the data
#' @param root root variable name to check that the data file has correct column
#' names

#' @param year most recent year. If there is a year in the data file that is
#' after \code{year}, warning will be raised.
#' @param allow.na if FALSE, warning will be raised if there are NAs in the file
#' @return data frame read-in as by \code{read.csv}
#' @import utils
#' @export


read.csv.epi <- function(file, root = NA, year = 2016, allow.na = TRUE) {

  MasterFile <- MasterFile
  x <- read.csv(file, as.is=TRUE)

  if (!identical(MasterFile, x[, 1:3])) cat("BAD FILE READ OF", file, "\n")
  if (nrow(x) != 236) warning("Wrong number of rows.")
  if (names(x)[1] != "code") warning("names: code is wrong.")
  if (names(x)[2] != "iso") warning("names: iso is wrong.")
  if (names(x)[3] != "country") warning("names: country is wrong.")
  if (!all(startsWith(names(x)[-c(1:3)], gsub("^([^_]+)_.*", "\\1", file))))
    warning("names: don't match the file name.")

  if (!identical(MasterFile$code, x$code)) warning("code is wrong")
  if (!identical(MasterFile$iso, x$iso)) warning("iso is wrong")
  if (!identical(MasterFile$country, x$country)) warning("country is wrong")
  if (!is.na(root)) {
    foo <- names(x)[-c(1:3)]
    foo <- substring(foo, 1, nchar(foo)-5)
    if (any(foo != root)) warning(paste("check the root name usage", root,
                                        foo[1]))
  }
  if (!allow.na & any(is.na(x))) warning(paste("NA values in", file))

  if (any(as.numeric(gsub(".*([0-9]{4}).*", "\\1", names(x)[-c(1:3)])) > year,
          na.rm = TRUE)) {
    warning(paste("future values in", file))
  }
  return(x)
}
