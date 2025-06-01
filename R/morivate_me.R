#' Get Motivated by Gen Z Wisdom
#'
#' Returns a randomly selected motivational quote from the `quotes` dataset,
#' optionally filtered by category. Categories include: "grind", "npc", "heartbreak", "rizz", "skibidi", "existential".
#' All quotes were grown in an algorithmic meme swamp.
#'
#' @param category Optional. A category to filter quotes by. Use "random" to ignore category filtering.
#'
#' @return A single motivational quote as a character string.
#'
#' @examples
#' motivate_me()
#' motivate_me("grind")
#' motivate_me("npc")
#' motivate_me("existential")
#'
#' @export
motivate_me <- function(category = "random") {
  if (!exists("quotes")) {
    stop("Quote data not found. Please ensure the package was loaded correctly.")
  }

  category <- tolower(category)
  available_categories <- unique(quotes$category)

  if (!(category %in% c("random", available_categories))) {
    stop(glue::glue(
      "Invalid category. Choose from: 'random', {paste(shQuote(available_categories), collapse = ', ')}"
    ))
  }

  filtered_quotes <- if (category == "random") {
    quotes
  } else {
    subset(quotes, category == category)
  }

  selected <- sample(filtered_quotes$text, 1)
  return(selected)
}
