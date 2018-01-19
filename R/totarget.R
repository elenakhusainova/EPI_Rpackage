#' Scales the data with target values
#'
#' For positive polarity, high target is considered good and low target bad.
#' Values under bad target are converted to 0 and values over good target are
#' converted to 100. All other values are scaled in between 0 and 100. The
#' opposite is performed for negative polarity.
#'
#' @usage totarget(x, low, high, polarity = "negative")
#'
#' @param x data to be scaled with target values
#' @param low low target value
#' @param high high target value
#' @param polarity polarity, either positive or negative
#' @return scaled data
#' @export
#'

totarget <- function(x, low, high, polarity = "negative") {

  if (low >= high) stop("Check targets!")
  if (!(polarity %in% c("negative", "positive"))) stop("Check polarity!")

  x_num <- x[, -c(1:3)]
  x_num[x_num == -8888] <- NA # Missing values for nonexisting countries

  x_num <- 100 * (x_num-low)/(high-low)
  x_num[x_num >= 100] <- 100
  x_num[x_num <= 0] <- 0

  if (polarity == "negative") x_num <- 100 - x_num

  # Put -8888 back into places
  x_num[is.na(x_num)] <- x[, -c(1:3)][is.na(x_num)]

  x[, -c(1:3)] <- x_num

  return(x)
}
