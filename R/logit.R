#' Log or Log-inverse transformation
#'
#' @description Log tranforms the data with an appropriate adjustments
#'
#' @details EPI data frame follows the 2018 Masterfile convention with the first
#' 3 columns being [code, iso, country] and the following columns being
#' VARIABLE.STEP.YEAR (e.g. MPA.trf.2005)
#'
#' @usage logit(x, shift, inverse = FALSE, maxval = 100)
#' @param x EPI data frame to be log tranformed. There cannot be negative values
#' or missing values coded as -9999/-8888.
#' @param shift shift size, zero by default. Can be set to "auto", then it will
#' be derived from the data
#' @param inverse loginverse or not
#' @param maxval value to subtract data values from
#' @return log-transformed EPI data frame
#'
#' @export

logit <- function(x, shift, inverse = FALSE, maxval = 100) {

  x_num <- x[, -c(1:3)]

  # Inverse if necessary:
  if (inverse) {
    if (!is.numeric(maxval)) stop("Argument maxval must be numeric!\n")
    if (maxval < max(x_num, na.rm = TRUE)) stop("Provided maxval is smaller than data maximum!\n")
    if (any(x_num < 0, na.rm = TRUE)) stop("Ask Zach: there are negative values before the inversion!\n")
    x_num <- maxval - x_num
  }

  # Check for negative values:
  if (any(x_num < 0, na.rm = TRUE)) stop("There are negative values in the data frame!\n")

  # Shift:
  if (missing(shift)) {
    shift <- ifelse(any(x_num == 0, na.rm = TRUE),
                    min(x_num[x_num != 0], na.rm = TRUE), # smallest positive number
                    0)
  }
  else {
    if (!is.numeric(shift)) stop("Provide the appropriate shift size!\n")
    if (shift < 0) stop("The shift is negative!\n")
    if (shift == 0 & any(x_num == 0, na.rm = TRUE)) stop("The shift is zero and there are zeros in the data\n")
  }

  # Log-transform
  x[, -c(1:3)] <- log(x_num + shift)

  return(list(shift = shift, x = x))
}
