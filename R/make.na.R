#' Make NA
#'
#' Converts EPI missing value codes to NA for all files in the given folder.
#' Resulting files are put in the same folder with '_na' added to their original
#' names.
#'
#' @usage make.na(folder)
#' @param folder folder with .csv files
#' @export

make.na <- function(folder) {

  files <- list.files(folder, pattern = ".csv$")
  # cat("Making _na files for the below files in", folder, "\n")
  # print(files)
  for (file in files) {
    if (regexpr("_na", file, fixed = TRUE) <= 0) {
      # cat("Making _na files for", folder, file, "\n")
      x <- read.csv(file.path(folder, file), as.is = TRUE)
      x[x == -9999] <- NA # Missing values for existing countries
      x[x == -8888] <- NA # Filling values for newly added countries
      x[x == -7777] <- NA # Filling values for blown out time series
      x[x == -6666] <- NA # Filling values for years missing due to missing normalizers
      # if (any(x < 0, na.rm = TRUE)) message(paste(folder, file,
      #                                             "contains negative values"))
      nafile <- gsub(".csv", "_na.csv", file)
      write.csv(x, file.path(folder, nafile), row.names=FALSE)
    }
  }
  # cat("\nMaking _na files complete!\n")
}

