library(dplyr)
data(weather_mittadalen)

weather_2019 <- weather_mittadalen %>%
  dplyr::filter(year == 2019)

with(weather_2019, find_date_permanent_snow(date, snow_depth, temp.window.size = 14))

# not run
with(weather_2019, find_date_permanent_snow(date, snow_depth, temp.window.size = 14,
                                            set_start = lubridate::ymd_h("2018-11-01 00")))

