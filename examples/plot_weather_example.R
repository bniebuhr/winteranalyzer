library(ggplot2)
library(dplyr)
data(weather_mittadalen)

weather_2019 <- weather_mittadalen %>%
  dplyr::filter(year == 2019)

weather_condition_2019 <-
  with(weather_2019, analyze_weather(date, snow_depth, prec, temp_min, temp_max,
                                     temp_avg, start = "first_permanent_snow",
                                     plot_first_snow = T))

plot_weather(weather_condition_2019, term = c("snow_depth", "precip"))
plot_weather(weather_condition_2019, term = c("snow_de", "cum"))
plot_weather(weather_condition_2019, term = c("snow_de", "cum"), add_events = "events3")
plot_weather(weather_condition_2019, term = c("snow_de", "cum", "snow_prec_ratio"))
plot_weather(weather_condition_2019, term = c("snow_de", "cum", "snow_prec_ratio"),
             factor_mult = c(1,1,100))
plot_weather(weather_condition_2019, term = c("snow_de", "cum", "snow_prec_ratio"),
             factor_mult = c(.01,.01,1), add_events = "events3")
