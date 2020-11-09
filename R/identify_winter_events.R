#' Identify harsh weather winter events
#'
#' This functions identifies the winter events related to temperature and precipitation described in
#' Lundqvist et al. (2007). The functions uses daily temperature (min, max, or average, in degrees Celcius) and
#' precipitation (in mm) to identify events that may be harsh weather conditions for reindeer husbandry.
#'
#' @details
#' The events are:
#'   * Events 1 and 2 are still to be implemented;
#'   * Event 3: Day when max temp > 2, min temp < -2, and precipitation > 3 mm;
#'   * Event 4: Day when average temp > 1, precipitation > 3 mm, followed by day with average temp < -1.
#'
#' @param date date object. vector. Vector of dates in a time series of a given winter.
#' @param temp_min numeric. vector. Minimum daily temperature, in Celsius.
#' @param temp_max numeric. vector. Minimum daily temperature, in Celsius.
#' @param temp_avg numeric. vector. Average daily temperature, in Celsius.
#' @param precipitation numeric. vector. Precipitation accumulated each day, in mm.
#' @param temp_min_thr numeric. Threshold of minimum temperature, in Celsius, used for event 3.
#' @param temp_max_thr numeric. Threshold of maximum temperature, in Celsius, used for event 3.
#' @param prec_thr numeric. Threshold of precipitation, in mm, used for events 3 and 4.
#'
#' @return A list with the elements:
#' \describe{
#'   \item{events}{time series with 0 for days with no event and 1 with days when the event occurred.}
#'   \item{n_days}{number of days when the event occurred.}
#'   \item{n_events}{number of events, by aggregating subsequent days when it repeatedly happened.}
#'   \item{duration_events}{duration of each event, in days.}
#'   \item{event_dates}{starting date of each event.}
#'   \item{n_event_date}{time series with the counter of the number of events. Similar to events.}
#' } These elements are calculated for both the whole winter and only the first half of the winter.
#'
#' @example examples/identify_winter_events_example.R
#'
#' @references Lundqvist, H., Norell, L., Danel, O. 2007. Multivariate characterisation of environmental
#' conditions for reindeer husbandry in Sweden. Rangifer, 27(1): 5-23.
#'
#' @export
#' @rdname identify_winter_events
identify_winter_event3 <- function(date, temp_min, temp_max, precipitation,
                                   temp_min_thr = -2, temp_max_thr = 2,
                                   prec_thr = 3) {

  # fill gaps in the time series if it is necessary

  # has an event 3 occurred in this day?
  event3 <- ifelse(temp_min < temp_min_thr & temp_max > temp_max_thr & precipitation > prec_thr, 1, 0)

  # identify number of days, events and duration, during the whole winter

  # total days with this kind of event
  n_days <- sum(event3, na.rm = T)

  # events and duration
  events_temp <- rle(event3)

  # number of events
  n_events3 <- sum(events_temp$values, na.rm = T)
  # duration of each event
  duration_events3 <- events_temp$lengths[events_temp$values > 0]
  # event number in the time series
  sequence_events3 <- ifelse(events_temp$values == 0, 0, cumsum(ifelse(events_temp$values == 0, 0, 1)))
  n_event3_date <- rep(sequence_events3, events_temp$lengths)
  # dates of the start of the events
  start_index <- c(1, 1 + cumsum(events_temp$lengths))
  start_index_event3 <- start_index[-length(start_index)][events_temp$values > 0]
  event_dates <- date[start_index_event3]

  # identify number of events and duration, only in the first half of the winter
  second_half <- (length(date)/2):length(date)
  event3_begin <- event3
  event3_begin[second_half] <- 0
  n_days_begin <- sum(event3_begin, na.rm = T)

  events_temp_begin <- rle(event3_begin)

  # number of events
  n_events3_begin <- sum(events_temp_begin$values, na.rm = T)
  # duration of each event
  duration_events3_begin <- events_temp_begin$lengths[events_temp_begin$values > 0]
  # event number in the time series
  sequence_events3_begin <- ifelse(events_temp_begin$values == 0, 0, cumsum(ifelse(events_temp_begin$values == 0, 0, 1)))
  n_event3_date_begin <- rep(sequence_events3_begin, events_temp_begin$lengths)
  # dates of the start of the events
  start_index_begin <- c(1, 1 + cumsum(events_temp_begin$lengths))
  start_index_event3_begin <- start_index[-length(start_index)][events_temp_begin$values > 0]
  event_dates_begin <- date[start_index_event3_begin]

  return(list(events3 = event3, n_days = n_days, n_events3 = n_events3,
              duration_events3 = duration_events3,
              event_dates = event_dates, n_event3_date = n_event3_date,
              events3_begin = event3_begin, n_days_begin = n_days_begin, n_events3_begin = n_events3_begin,
              duration_events3_begin = duration_events3_begin,
              event_dates_begin = event_dates_begin, n_event3_date_begin = n_event3_date_begin))
}

#' @export
#' @param temp_avg_day numeric. Threshold of average temperature in the day, in Celsius, used for event 4.
#' @param temp_avg_next_day numeric. Threshold of average temperature in the following day, in Celsius, used for event 4.
#' @rdname identify_winter_events
identify_winter_event4 <- function(date, temp_avg, precipitation,
                                   temp_avg_day = +1, temp_avg_next_day = -1,
                                   prec_thr = 3) {

  # fill gaps in the time series if it is necessary

  # has an event 4 occurred in this day?
  event4 <- sapply(1:(length(date)-1), function(i)
    ifelse(temp_avg[i] > temp_avg_day & temp_avg[(i+1)] < temp_avg_next_day & precipitation[i] > prec_thr, 1, 0))
  # no possibility to assess that in the last day
  event4 <- c(event4, NA)

  # total days with this kind of event
  n_days <- sum(event4, na.rm = T)

  # identify number of events and duration
  events_temp <- rle(event4[-length(event4)])

  # number of events
  n_events4 <- sum(events_temp$values, na.rm = T)
  # duration of each event
  duration_events4 <- events_temp$lengths[events_temp$values > 0]
  # event number in the time series
  sequence_events4 <- ifelse(events_temp$values == 0, 0, cumsum(ifelse(events_temp$values == 0, 0, 1)))
  n_event4_date <- rep(sequence_events4, events_temp$lengths)
  # dates of the start of the events
  start_index <- c(1, 1 + cumsum(events_temp$lengths))
  start_index_event4 <- start_index[-length(start_index)][events_temp$values > 0]
  event_dates <- date[start_index_event4]

  # identify number of events and duration, only in the first half of the winter
  second_half <- (length(date)/2):length(date)
  event4_begin <- event4
  event4_begin[second_half] <- 0
  n_days_begin <- sum(event4_begin, na.rm = T)

  events_temp_begin <- rle(event4_begin)

  # number of events
  n_events4_begin <- sum(events_temp_begin$values, na.rm = T)
  # duration of each event
  duration_events4_begin <- events_temp_begin$lengths[events_temp_begin$values > 0]
  # event number in the time series
  sequence_events4_begin <- ifelse(events_temp_begin$values == 0, 0, cumsum(ifelse(events_temp_begin$values == 0, 0, 1)))
  n_event4_date_begin <- rep(sequence_events4_begin, events_temp_begin$lengths)
  # dates of the start of the events
  start_index_begin <- c(1, 1 + cumsum(events_temp_begin$lengths))
  start_index_event4_begin <- start_index[-length(start_index)][events_temp_begin$values > 0]
  event_dates_begin <- date[start_index_event4_begin]

  return(list(events4 = event4, n_days = n_days, n_events4 = n_events4,
              duration_events4 = duration_events4,
              event_dates = event_dates, n_event4_date = n_event4_date,
              events4_begin = event4_begin, n_days_begin = n_days_begin, n_events4_begin = n_events4_begin,
              duration_events4_begin = duration_events4_begin,
              event_dates_begin = event_dates_begin, n_event4_date_begin = n_event4_date_begin))

  return(event4)
}
