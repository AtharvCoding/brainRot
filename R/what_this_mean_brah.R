#' Explain what an indicator means (brainrot edition)
#'
#' Given an indicator name, this function returns a helpful (and chaotic) description.
#'
#' @param indicator A string name of the indicator you want explained.
#'
#' @return A character string with the indicator's description.
#' @export
what_this_mean_brah <- function(indicator) {
  if (!exists("dictionary")) {
    stop("dictionary not found. Make sure it's loaded with the package.")
  }

  entry <- dictionary[dictionary$indicator_name == indicator, ]

  if (nrow(entry) == 0) {
    return(paste0("bro idk what '", indicator, "' means maybe it's a typo?"))
  } else {
    return(paste0(indicator, "`: ", entry$description))
  }
}
