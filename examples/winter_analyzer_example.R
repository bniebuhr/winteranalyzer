library(dplyr)
data(weather_mittadalen)

weather_2019 <- weather_mittadalen %>%
  dplyr::filter(year == 2019)

weather_condition_2019 <-
  with(weather_2019, analyze_weather(date, snow_depth, prec, temp_min, temp_max,
                                     temp_avg, start = "first_permanent_snow",
                                     plot_first_snow = T))
weather_condition_2019

weather_condition_2019 %>%
  ggplot(aes(date)) +
  geom_line(aes(y = snow_depth, color = "snow depth")) +
  geom_line(aes(y = cumulative_precitation, color = "cum prec")) +
  geom_line(aes(y = 100*prec_snow_ratio, color = "100*cum prec/snow depth")) +
  geom_hline(yintercept = 100, linetype = 2) +
  ylim(0, 250) +
  labs(x = "Date", y = "Amount (mm)", color = "")
