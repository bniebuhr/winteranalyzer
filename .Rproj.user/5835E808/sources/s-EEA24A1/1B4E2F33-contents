library(ggplot2)
data(weather_mittadalen)

ggplot(weather_mittadalen, aes(date)) +
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = snow_depth, color = "Snow depth (mm)"))  +
  geom_line(aes(y = prec, color = "Precipitation (mm)")) +
  labs(x = "Date", y = "", color = "Measure") +
  theme(legend.position = "bottom")

data(weather_tassasen)

ggplot(weather_tassasen, aes(date)) +
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = snow_depth, color = "Snow depth (mm)"))  +
  geom_line(aes(y = prec, color = "Precipitation (mm)")) +
  labs(x = "Date", y = "", color = "Measure") +
  theme(legend.position = "bottom")
