# example
library(witch)
library(dplyr)
data("weather_tassasen")

weather <- weather_tassasen %>%
  dplyr::filter(year == 2015)

# new function
?identify_winter_eventX
with(weather, identify_winter_eventX(date, temp_max = temp_max, temp_min = temp_min,
                                     precipitation = prec,
                                     temp_max_thr_day = 1, temp_min_thr_next_day = -2,
                                     number_days_after = 3))
# 6 events in the first half

# we can change the values
with(weather, identify_winter_eventX(date, temp_max = temp_max, temp_min = temp_min,
                                     precipitation = prec,
                                     temp_max_thr_day = 0, temp_min_thr_next_day = -2,
                                     number_days_after = 3))
# 7 events if we change max temp to above zero

# this is also within the function analyze_weather
(weather_analyzed <- with(weather, analyze_weather(date, snow_depth, prec, temp_min,
                                                  temp_max, temp_avg,
                                                  plot_first_snow = F)))

plot_weather(weather_analyzed, term = c("snow_dep", "cum"),
             add_events = "eventsX")

# now we can change the values
with(weather, analyze_weather(date, snow_depth, prec, temp_min,
                              temp_max, temp_avg, #start = "start",
                              plot_first_snow = F,
                              evX_temp_max_thr_day = 0, evX_temp_min_thr_next_day = -2,
                              evX_number_days_after = 3)) %>%
  plot_weather(term = c("snow_dep", "cum"),
               add_events = "eventsX")
