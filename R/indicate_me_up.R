#' List all available indicators
#'
#' This function returns the names of all indicators available in the dataset.
#' Use this to find what columns you can explore.
#'
#' @return A character vector of indicator names.
#' @export
indicate_me_up <- function() {
  if (!exists("dictionary")) {
    stop("dictionary not found. Make sure it's loaded with the package.")
  }
  return(unique(dictionary$indicator_name))
}
