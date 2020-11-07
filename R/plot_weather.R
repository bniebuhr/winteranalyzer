#' Plot weather curves for a given winter
#'
#' This function plots weather variables and indices calculated with the function
#' \code{\link[winteranalyzer]{analyze_weather}}.
#'
#' @param weather_analyzed data.frame. output `data.frame` from the \code{\link[winteranalyzer]{analyze_weather}}
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
                         add_events = c("", "events3", "events4")[1],
                         first_half = TRUE,
                         cols = c(),
                         units = c()) {
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
    labs(x = "Date", y = "Measure", color = "")

  # add each term if this is an argument of the function
  cont <- 1
  for(i in term) {
    index <- which(grepl(i, possible_terms))[1]
    p <- p + geom_line(aes_q(y = as.name(possible_terms[index]),
                               color = possible_terms[index]))

    if(grepl("ratio", i)) {
      col_number <- which(grepl(i, colnames(weather_df)))
      p <- p + geom_hline(yintercept = factor_mult[(col_number-1)], linetype = 2)
    }

  }

  if(add_events == "events3" | add_events == "events4") {
    dates <- ifelse(first_half, weather_analyzed[[add_events]]$event_dates_begin,
                    weather_analyzed[[add_events]]$event_dates)
    p <- p + geom_vline(xintercept = dates, linetype = 3)
  }

  p
}

### Add option to select the sequence of colors
