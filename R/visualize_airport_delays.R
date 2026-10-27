#' Visualize mean flight delays by airport
#'
#' Plots the mean arrival delay of the flights to each destination airport
#' in the `nycflights13` data, placed by the airport's longitude and latitude.
#'
#' @return A ggplot object.
#'
#' @examples
#' visualize_airport_delays()
#'
#' @importFrom dplyr .data
#' @export
visualize_airport_delays <- function() {
  # Calculates the mean arrival delay for each destination airport
  delays <- nycflights13::flights |>
    dplyr::filter(!is.na(.data$arr_delay)) |>
    dplyr::group_by(.data$dest) |>
    dplyr::summarise(mean_delay = mean(.data$arr_delay)) |>
    dplyr::inner_join(nycflights13::airports, by = c("dest" = "faa"))

  # Plots each airport at its location, coloured by its mean delay
  ggplot2::ggplot(
    delays,
    ggplot2::aes(x = .data$lon, y = .data$lat, colour = .data$mean_delay)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Mean arrival delay by destination airport",
      x = "Longitude", y = "Latitude", colour = "Mean delay (min)"
    )
}