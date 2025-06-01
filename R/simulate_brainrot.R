#' Simulate Brainrot from Skibidi Consumption
#'
#' Given the number of hours watching Skibidi content, this function simulates IQ decay over time using an exponential decay model, and visualizes the mental decline using a ggplot. Side effects may include statistical embarrassment.
#'
#' @param hours Number of hours spent watching Skibidi (numeric, must be >= 0).
#'
#' @return A list containing:
#' \describe{
#'   \item{iq}{The final IQ score after brainrot.}
#'   \item{plot}{A ggplot object showing IQ drop over time.}
#' }
#'
#' @examples
#' # Basic usage
#' simulate_brainrot(5)
#'
#' # Capture and view the plot
#' result <- simulate_brainrot(10)
#' result$plot
#'
#' # One-liner to show plot directly
#' print(simulate_brainrot(3)$plot)
#'
#' @export
simulate_brainrot <- function(hours = 5)
  {
  if (!is.numeric(hours) || hours < 0)
  {
    stop("are u srs rn bro dont pmo, use a valid number frfr")
  }

  base_iq <- 100
  decay_rate <- 0.12

  times <- seq(0, hours, by = 0.5)
  iq <- base_iq * exp(-decay_rate * times)

  df <- data.frame(time = times, iq = iq)

  final_iq <- round(tail(iq, 1), 1)

  message(glue::glue(" After {hours} hours of Skibidi, your IQ is {final_iq}. Seek help."))

  plot <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = iq)) +
    ggplot2::geom_line(color = "#FF0055", size = 2, linetype = "twodash") +
    ggplot2::geom_point(color = "#FFFF00", shape = 8, size = 3) +  # star points
    ggplot2::labs(
      title = " IQ Decay vs Skibidi Overdose",
      subtitle = "The more you scroll, the more you crumble.",
      x = "Hours of Exposure",
      y = " Remaining IQ"
    ) +
    ggplot2::theme_minimal(base_family = "Comic Sans MS") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#000000", color = NA),
      panel.background = ggplot2::element_rect(fill = "#111111", color = NA),
      plot.title = ggplot2::element_text(color = "#FF00FF", size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "#00FFFF", size = 12, face = "italic"),
      axis.text = ggplot2::element_text(color = "#FFDD00", size = 10),
      axis.title = ggplot2::element_text(color = "#00FF00", size = 12)
    )

  return(invisible(list(iq = final_iq, plot = plot)))
}
