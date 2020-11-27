#' Plot weather curves for a given winter
#'
#' This function plots weather variables and indices calculated with the function
#' \code{\link[witch]{analyze_weather}}. It returns a ggplot plot, which means you can
#' use it together with other ggplot2 functions (see examples).
#'
#' @param weather_analyzed data.frame. output `data.frame` from the \code{\link[witch]{analyze_weather}}
#' function. This data.frame may contain time series of snow_depth, precipitation, accumulated
#' precipitation, temperature (min, max, avg), and ratio and difference of snow_depth and cumulative
#' precipitation (and vice-versa).
#' @param term character. term to be plotted.
#' @param factor_mult numeric, vector. numeric factor to be multipled by each variable (defined
#' in the argument `term`), so that all variables are plotted in the same scale
#' @param cols character or number. vector. sequence of colors for plotting each term, in the same order as term.
#' @param units character. vector. vector of units of each of the terms.
#'
#' @return a ggplot with the terms plotted.
#'
#' @example examples/plot_weather_example.R
#'
#' @export
plot_weather <- function(weather_analyzed,
                         term = c("snow_depth", "precip", "temp_min", "temp_max", "temp_avg",
                                   "cumulative_precitation", "prec_snow_ratio", "prec_snow_diff",
                                   "snow_prec_ratio", "snow_prec_diff")[c(1,2)],
                         factor_mult = rep(1, length(term)),
                         add_events = c("", "events3", "events4", "eventsX")[1],
                         first_half = TRUE,
                         cols = c(),
                         units = c(),
                         title = "") {
  # possible terms
  possible_terms <- colnames(weather_analyzed$weather_indices)

  # check if any of the terms correspond to more than one possible term, raise an error if it does

  # copy weather df
  weather_df <- weather_analyzed$weather_indices %>%
    dplyr::select(1, which(grepl(paste(term, collapse = "|"), possible_terms)))
  # weather_df %>% summary()
  weather_df[2:ncol(weather_df)] <- mapply(function(a, b) a*b, weather_df[2:ncol(weather_df)], factor_mult)

  # initialize plot
  p <- ggplot(weather_df, aes(date)) +
    labs(x = "Date", y = "Measure", colour = "Variable")

  # add each term if this is an argument of the function
  cont <- 1
  for(i in term) {
    index <- which(grepl(i, possible_terms))[1]
    p <- p + geom_line(aes_q(y = as.name(possible_terms[index]),
                               colour = possible_terms[index]))

    if(grepl("ratio", i)) {
      col_number <- which(grepl(i, colnames(weather_df)))
      p <- p + geom_hline(yintercept = factor_mult[(col_number-1)], linetype = 2)
    }

  }

  # events 3 and 4
  if(first_half) {
    dates3 <- weather_analyzed[["events3"]]$event_dates_begin
    dates4 <- weather_analyzed[["events4"]]$event_dates_begin
    datesX <- weather_analyzed[["eventsX"]]$event_dates_begin
  } else {
    dates3 <- weather_analyzed[["events3"]]$event_dates
    dates4 <- weather_analyzed[["events4"]]$event_dates
    datesX <- weather_analyzed[["eventsX"]]$event_dates
  }
  # dates3 <- ifelse(first_half, weather_analyzed[["events3"]]$event_dates_begin,
  #                              weather_analyzed[["events3"]]$event_dates)
  # dates4 <- ifelse(first_half, weather_analyzed[["events4"]]$event_dates_begin,
  #                  weather_analyzed[["events4"]]$event_dates)
  # datesX <- if_else(rep(first_half, 2), weather_analyzed[["eventsX"]]$event_dates_begin,
  #                  weather_analyzed[["eventsX"]]$event_dates)
  # events <- data.frame(dates = c(dates3, dates4), event_type = c(rep("Ice crust 3", length(dates3)),
  #                                                         rep("Ice crust 4", length(dates4))))

  if("events3" %in% add_events) {
    p <- p + geom_vline(xintercept = dates3, linetype = 2, colour = "blue")
      # geom_vline(aes(xintercept = dates, colour = event_type), linetype = 2,
      #            data = events, show.legend = F)
  }
  if("events4" %in% add_events) {
    p <- p + geom_vline(xintercept = dates4, linetype = 2, colour = "red")
  }
  if("eventsX" %in% add_events) {
    p <- p + geom_vline(xintercept = datesX, linetype = 2, colour = "darkgreen")
  }

  p +
    labs(title = title)
}
