#' Analysis of winter conditions based on meteorological variables
#'
#' The function \code{analyze_weather} calculates some proxies for weather conditions based on
#' time series of snow depth, precipitation, and minimum, maximum, and average
#' temperature for a given year. The functions \code{n_days},
#' \code{n_events}, and \code{duration_events} calculate the number of days,
#' number of events, and duration of events of type 3 and 4 based on
#' the output of the function \code{analyze_weather}.
#'
#' @param date date object. vector. Vector of dates in a time series of a given winter.
#' @param snow_depth numeric. vector. Daily snow depth, in mm.
#' @param precipitation numeric. vector. Precipitation accumulated each day, in mm.
#' @param temp_min numeric. vector. Minimum daily temperature, in Celsius.
#' @param temp_max numeric. vector. Minimum daily temperature, in Celsius.
#' @param temp_avg numeric. vector. Average daily temperature, in Celsius.
#' @param start character. Whether the analysis should be done from the
#' "first_permanent_snow", from the "first_date" of the time series, or from
#' "other_date". In the first case, it uses the function
#' \code{\link[winteranalyzer]{find_date_permanent_snow}} to search for when the
#' snow comes and stays. In the last case, an initial date must be set by the
#' parameter \code{set_start}.
#' @param plot_first_snow logical. Whether or not to plot the time series of snow
#' and the estimated start for analyzing the time series
#' @param set_start POSIX. Starting date of the analysis, in case \code{start = "other_date"}.
#' @param ... other parameters for the functions
#' \code{\link[winteranalyzer]{find_date_permanent_snow}},
#' \code{\link[winteranalyzer]{identify_winter_event3}}, or
#' \code{\link[winteranalyzer]{identify_winter_event4}}.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{\code{weather_indices}}{a data.frame with the original data plus time series of some indices calculated:
#'   cumumative precipitation, the ratio between snow and cumulative precipitation,
#'   the ratio between cumulative precipitation and snow, the difference between
#'   cumulative precipitation and snow, and when events 3 and 4 happened.}
#'   \item{\code{events3}}{a list with information about events 3. See the output from the function
#'   \code{\link[winteranalyzer]{identify_winter_event3}}.}
#'   \item{\code{events4}}{a list with information about events 4. See the output from the function
#'   \code{\link[winteranalyzer]{identify_winter_event4}}.}
#' }
#'
#' @example examples/analyze_weather_example.R
#'
#' @references Lundqvist, H., Norell, L., Danel, O. 2007. Multivariate characterisation of environmental
#' conditions for reindeer husbandry in Sweden. Rangifer, 27(1): 5-23.
#'
#' @export
#' @rdname analyze_weather
analyze_weather <- function(date, snow_depth, precipitation,
                            temp_min, temp_max, temp_avg,
                            start = c("first_permanent_snow", "first_date", "other date")[1],
                            plot_first_snow = FALSE,
                            set_start = NULL, ...) {

  # define starting date
  if(start == "first_permanent_snow") {
    start = find_date_permanent_snow(date, snow_depth, plot = plot_first_snow, ...)
  } else if(start == "other date") {
    start = find_date_permanent_snow(date, snow_depth, plot = plot_first_snow,
                                     set_start = set_start, ...)
  } else {
    start = date[1]
  }

  # calculate precipitation and cumulative precipitation from the start date
  precipitation_start <- ifelse(date < start, 0, precipitation)
  cum_prec <- cumsum(precipitation_start)

  # calculate indices - ratios and differences of snow depth and cumulative precipitation
  prec_snow_ratio <- cum_prec/snow_depth
  prec_snow_diff <- cum_prec - snow_depth
  snow_prec_ratio <- snow_depth/cum_prec
  snow_prec_diff <- snow_depth - cum_prec

  # calculate events 3 and 4
  events3 <- identify_winter_event3(date, temp_min, temp_max, precipitation_start, ...)

  events4 <- identify_winter_event4(date, temp_avg, precipitation_start, ...)

  # make a data.frame with all
  weather.indices <- tibble::tibble(date, snow_depth, precipitation_start,
                                    temp_min, temp_max, temp_avg,
                                    cumulative_precitation = cum_prec,
                                    prec_snow_ratio, prec_snow_diff,
                                    snow_prec_ratio, snow_prec_diff,
                                    events3 = events3$events3_begin,
                                    events4 = events4$events4_begin)

  return(list(weather_indices = weather.indices, events3 = events3, events4 = events4))
}

#' @export
#' @param weather_analyzed list. output list from the function \code{\link[winteranalyzer]{analyze_weather}}.
#' @param event numeric. Which type of event (3 or 4).
#' @param first_half logical. Whether to get events from the whole winter (FALSE) or
#' only from the first half of the winter (TRUE), when the effects of these events
#' may be worse.
#'
#' @rdname analyze_weather
n_days <- function(weather_analyzed, event = 3, first_half = TRUE) {
  if(event == 3) {
    if(first_half) return(weather_analyzed$events3$n_days_begin) else
      return(weather_analyzed$events3$n_days)
  } else if(event == 4) {
    if(first_half) return(weather_analyzed$events4$n_days_begin) else
      return(weather_analyzed$events4$n_days)
  } else
    NA
}

#' @export
#' @rdname analyze_weather
n_events <- function(weather_analyzed, event = 3, first_half = TRUE) {
  if(event == 3) {
    if(first_half) return(weather_analyzed$events3$n_events3_begin) else
      return(weather_analyzed$events3$n_events3)
  } else if(event == 4) {
    if(first_half) return(weather_analyzed$events4$n_events4_begin) else
      return(weather_analyzed$events4$n_events4)
  } else
    NA
}

#' @export
#' @rdname analyze_weather
duration_event <- function(weather_analyzed, event = 3, first_half = TRUE) {
  if(event == 3) {
    if(first_half) return(weather_analyzed$events3$duration_events3_begin) else
      return(weather_analyzed$events3$duration_events3)
  } else  if(event == 4) {
    if(first_half) return(weather_analyzed$events4$duration_events4_begin) else
      return(weather_analyzed$events4$duration_events4)
  } else
    NA
}
