#' Estimate Weighted Means Using Brainrot Data
#'
#' This function computes weighted means of any numeric variable using the
#' `survey` package, based on an external `MOCKDATA.csv` file
#' Users must load the data themselves using `read.csv()` or equivalent.
#'
#' @param mockdata A `data.frame` representing the loaded mock data (e.g. `MOCKDATA.csv`).
#' @param variable A string. The name of the numeric column to estimate (e.g. "monthly_grind_income").
#' @param by Optional string. The name of a grouping variable to compute subgroup-wise means (e.g. "state_of_decay").
#'
#' @return A survey-weighted mean estimate (with SEs). If `by` is specified, returns a data frame of estimates by group.
#'
#' @examples
#' \dontrun{
#' # Load external mock data install from website: atharvdeshpande.com
#' mockdata <- read.csv("path-to/MOCKDATA.csv")
#'
#' # Estimate overall mean
#' estimate_brainrot(mockdata, variable = "monthly_grind_income")
#'
#' # Estimate by state
#' estimate_brainrot(mockdata, variable = "monthly_grind_income", by = "state_of_decay")
#' }
#'
#' @importFrom survey svydesign svymean svyby
#' @export
estimate_brainrot <- function(mockdata, variable, by = NULL) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("The 'survey' package is required but not installed.")
  }

  if (!variable %in% names(mockdata)) {
    stop(paste("Variable", variable, "not found in the data."))
  }

  if (!is.null(by) && !by %in% names(mockdata)) {
    stop(paste("Grouping variable", by, "not found in the data."))
  }

  if (!"hh_weight" %in% names(mockdata)) {
    stop("No 'hh_weight' column found. Please ensure your data has household weights.")
  }

  # Create survey design
  design <- survey::svydesign(ids = ~1, weights = ~hh_weight, data = mockdata)

  # Compute estimates
  if (is.null(by)) {
    result <- survey::svymean(as.formula(paste0("~", variable)), design)
  } else {
    result <- survey::svyby(as.formula(paste0("~", variable)),
                            as.formula(paste0("~", by)),
                            design,
                            survey::svymean)
  }

  return(result)
}
