library(tidyverse)
data(weather_mittadalen)

# select one year
weather <- weather_mittadalen %>%
  dplyr::filter(year == 2015)

# analyze weather
weather_condition <-
  with(weather, analyze_weather(date, snow_depth, prec, temp_min, temp_max,
                                     temp_avg, start = "first_permanent_snow",
                                     plot_first_snow = T))
weather_condition

n_days(weather_condition, event = 4)
n_events(weather_condition, event = 3)
n_events(weather_condition, event = 3, first_half = F)

# plot
weather_condition$weather_indices %>%
  ggplot(aes(date)) +
  geom_line(aes(y = snow_depth, color = "snow depth")) +
  geom_line(aes(y = cumulative_precitation, color = "cum prec")) +
  geom_line(aes(y = 100*prec_snow_ratio, color = "100*cum prec/snow depth")) +
  geom_hline(yintercept = 100, linetype = 2) +
  ylim(0, 250) +
  labs(x = "Date", y = "Amount (mm)", color = "")

# plot with events
weather_condition %>%
  plot_weather(term = c("snow_depth", "cum", "prec", "temp_max")) +
  geom_vline(xintercept = weather_condition$events3$event_dates, linetype = 2)

weather_condition$weather_indices %>%
  plot_weather(term = c("snow_depth", "cum", "prec", "temp_max")) +
  geom_vline(xintercept = weather_condition$events3$event_dates_begin, linetype = 2)


# for several years
n_weather <- weather_mittadalen %>%
  tidyr::nest(data = c(date, snow_depth, prec, temp_min, temp_max, temp_avg)) %>%
  dplyr::mutate(
    winter_conditions = purrr::map(data, ~ with(., analyze_weather(date, snow_depth, prec, temp_min, temp_max, temp_avg))),
    n_days3 = purrr::map(winter_conditions, ~ n_days(., event = 3)),
    n_days4 = purrr::map(winter_conditions, ~ n_days(., event = 4)),
    n_events3 = purrr::map(winter_conditions, ~ n_events(., event = 3)),
    n_events4 = purrr::map(winter_conditions, ~ n_events(., event = 4)),
    duration3 = purrr::map(winter_conditions, ~ duration_event(., event = 3)),
    duration4 = purrr::map(winter_conditions, ~ duration_event(., event = 4))
  ) %>%
  tidyr::unnest(n_days3:n_events4)

n_weather %>%
  dplyr::select(year, n_days3:n_events4) %>%
  tidyr::pivot_longer(cols = n_days3:n_events4, names_to = "what", values_to = "val") %>%
  ggplot(aes(x = year, y = val, color = what)) +
  geom_line()


