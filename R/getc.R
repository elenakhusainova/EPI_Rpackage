#' Return the correct country name
#'
#' @param country country name to be checked
#' @return correct country name
#' @examples
#' getc("USA")
#' @export

getc <- function(country) {

  dict <- dict
  sel <- which(dict$wrong == country)
  if (length(sel) != 1) {
    if (length(sel) == 0) message(paste(country, "not found"))
    if (length(sel)  > 1) message(paste(country, "found multiple times"))
    return(NA)
  } else return(dict$right[sel])
}

