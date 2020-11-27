library(ggplot2)
library(dplyr)
data(weather_mittadalen)

weather <- weather_mittadalen %>%
  dplyr::filter(year == 2015)

# events 3
with(weather, identify_winter_event3(date, temp_min, temp_max, prec))

# One can also change the parameters
with(weather, identify_winter_event3(date, temp_min, temp_max, prec,
                                     temp_max_thr = 0))

# events 4
with(weather, identify_winter_event4(date, temp_avg, prec))

# events X
with(weather, identify_winter_eventX(date, temp_max = temp_max, temp_min = temp_min,
                                     precipitation = prec,
                                     temp_max_thr_day = 1, temp_min_thr_next_day = -2,
                                     number_days_after = 3))

# visualize events 4 for this year
weather_condition <-
  with(weather, analyze_weather(date, snow_depth, prec, temp_min, temp_max,
                                temp_avg, start = "first_date",
                                plot_first_snow = T))

to_plot <- weather_condition

to_plot$weather_indices <- weather_condition$weather_indices %>%
  dplyr::mutate(temp_avg_next = c(temp_avg[-1], NA)) %>%
  dplyr::filter(lubridate::month(date) == 12)

to_plot %>%
  plot_weather(term = c("temp_avg", "prec", "temp_avg_next"), factor_mult = c(1,1)) +
  geom_hline(yintercept = c(-1,1), linetype = 2) +
  ylim(-10, 10)
