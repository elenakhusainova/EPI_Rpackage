#' Imputation by Interpolation and Extrapolation
#'
#' @description Imputes missing values by interpolation and extrapolation
#'
#' @details EPI data frame follows the 2018 Masterfile convention with the first
#' 3 columns being [code, iso, country] and the following columns being
#' VARIABLE.STEP.YEAR (e.g. MPA.trf.2005)
#'
#' @usage impute(x)
#'
#' @param x EPI data frame with -9999/-8888/-7777 codes
#' @return EPI data frame with missing values imputed by interpolation and
#' exterpolation and -8888 preserved
#'
#' @importFrom zoo na.approx
#' @export


impute <- function(x) {

  x[x == -9999] <- NA # Missing values for existing countries
  x[x == -8888] <- NA # Missing values for nonexisting countries
  x[x == -7777] <- NA # Missing values from blowing out years

  for (j in 1:nrow(x)) {

    # Linear interpolation of internal gaps
    curr_row <- unlist(x[j, -c(1:3)])

    if (sum(!is.na(curr_row)) > 1) {
      curr_row <- na.approx(curr_row, na.rm = FALSE, rule = 2)
    } else if (sum(!is.na(curr_row)) == 1) {
      curr_row[] <- mean(curr_row, na.rm = TRUE)
    }
    
    if (!(sum(!is.na(curr_row)) %in% c(0, length(curr_row)))) {
      stop("Imputation problem!")
    }

    if (all(is.na(curr_row))) curr_row[] <- -8888

    x[j, -c(1:3)] <- curr_row
  }

  return(x)
}
