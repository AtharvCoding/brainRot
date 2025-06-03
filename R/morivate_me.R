#' Return a random quote for the given category
#'
#' @param cat A character string matching one of the categories in the `quotes` dataset - grind, skibidi, npc, heartbreak, existential
#' @return A single character string (one random quote). If `cat` doesn’t exist, throws a warning and returns `NA`.
#' @export
motivate_me <- function(cat)
  {
  matched <- quotes[quotes$category == cat, "text"]
  if (length(matched) == 0) {
    warning(sprintf("No quotes found for category '%s'.", cat))
    return(NA_character_)
  }
  sample(matched, 1)
}
