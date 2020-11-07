library(ggplot2)
library(dplyr)
data(weather_mittadalen)

weather_2014 <- weather_mittadalen %>%
  dplyr::filter(year == 2014)

# events 3
with(weather_2014, identify_winter_event3(date, temp_min, temp_max, prec))

# events 4
with(weather_2014, identify_winter_event4(date, temp_avg, prec))

# visualize events 4 for this year
weather_condition_2014 <-
  with(weather_2014, analyze_weather(date, snow_depth, prec, temp_min, temp_max,
                                     temp_avg, start = "first_date",
                                     plot_first_snow = T))

weather_condition_2014$weather_indices %>%
  dplyr::mutate(temp_avg_next = c(temp_avg[-1], NA)) %>%
  dplyr::filter(lubridate::month(date) == 12) %>%
  plot_weather(term = c("temp_avg", "prec", "temp_avg_next"), factor_mult = c(1,1)) +
  geom_hline(yintercept = c(-1,1), linetype = 2) +
  ylim(-10, 5)
