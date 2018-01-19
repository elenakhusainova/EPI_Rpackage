#' Resizing the Data Frame
#'
#' @description Blows out or cuts the data frame with the given start and
#' end year. Nonexistent year columns are filled with NA. STEP TLA in column
#' names is changed to rsz.
#'
#' @details EPI data frame follows the 2018 Masterfile convention with the first
#' 3 columns being [code, iso, country] and the following columns being
#' VARIABLE.STEP.YEAR (e.g. MPA.trf.2005)
#'
#' @usage resize(x, start = 2000, end = 2017)
#'
#' @param x EPI data frame to be resized
#' @param start start year
#' @param end end year
#' @return resized EPI data frame
#'
#' @import utils
#' @export

resize <- function(x, start = 2000, end = 2017) {

  if (start > end) {
    stop("Start year is after end year!")
  }

  # Grab the variable name
  root <- substr(names(x)[4], 1, 3)

  # Create new data frame of the desired size with correct column names
  z <- as.data.frame(matrix(-7777, nrow = nrow(x), ncol = 3 + end - start + 1))
  names(z) <- c(names(x)[1:3], paste0(root, ".rsz.", c(start:end)))

  # Find the overlapping years
  xyears <- as.numeric(gsub("^.{8}", "", names(x)[4:ncol(x)]))
  zyears <- as.numeric(gsub("^.{8}", "", names(z)[4:ncol(z)]))

  overlap <- intersect(zyears, xyears) # years of data we have

  xcols <- paste0(gsub("(.*)[0-9]{4}", "\\1", names(x)[4]), overlap)
  zcols <- paste0(root, ".rsz.", overlap)

  # Fill in the new data frame with existing data
  z[, zcols] <- x[, xcols]
  z[, 1:3] <- x[, 1:3]

  return(z)
}
