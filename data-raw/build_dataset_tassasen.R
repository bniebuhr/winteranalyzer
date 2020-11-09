#' ---
#' title: 'Build dataset for winter conditions in Tassasen sameby, Sweden'
#' author: Bernardo Niebuhr
#' ---

library(magrittr)
library(dplyr)
library(ggplot2)

# read data
snow.ratan <- read.csv2("data/tassasen/Snodjup_Ratan.csv", skip = 10, header = TRUE, stringsAsFactors = F) %>% 
  dplyr::rename(date = 1, snow.ratan = 3) %>% 
  dplyr::mutate(date = lubridate::ymd(date, tz = "UTC"),
                snow.ratan = 100*as.numeric(snow.ratan)) %>% 
  dplyr::as_tibble()
snow.handsjo <- read.csv2("data/tassasen/Snodjup_Handsjo.csv", skip = 10, header = TRUE, stringsAsFactors = F) %>% 
  dplyr::rename(date = 1, snow.handsjo = 3) %>% 
  dplyr::mutate(date = lubridate::ymd(date, tz = "UTC"),
                snow.handsjo = 100*as.numeric(snow.handsjo)) %>% 
  dplyr::as_tibble()

temp.min_max.ytterhogdal <- read.csv2("data/tassasen/Temperatur_minmax_Ytterhogdal.csv", skip = 12, header = TRUE,
                                      stringsAsFactors = F) %>%
  dplyr::rename(date = 3, temp_min = 4, temp_max = 6) %>%
  dplyr::mutate(date = lubridate::ymd(date, tz = "UTC"),
                temp_min = as.numeric(temp_min),
                temp_max = as.numeric(temp_max)) %>% 
  dplyr::as_tibble()
temp.mean.ytterhogdal <- read.csv2("data/tassasen/Temperatur_dygn_Ytterhogdal.csv", skip = 11, header = TRUE,
                           stringsAsFactors = F) %>%
  dplyr::rename(date = 3, temp_avg = 4) %>%
  dplyr::mutate(date = lubridate::ymd(date, tz = "UTC"),
                temp_avg = as.numeric(temp_avg)) %>% 
  dplyr::as_tibble()

temp.mean.ytterhogdal %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>% 
  summarise(n = n())

precip.ratan <- read.csv2("data/tassasen/Rainfall_dygn_Ratan.csv", skip = 9, header = TRUE, 
                         stringsAsFactors = F) %>% 
  dplyr::rename(date = 3, prec.ratan = 4) %>% 
  dplyr::mutate(prec.ratan = as.numeric(prec.ratan),
                date = lubridate::ymd(date, tz = "UTC")) %>% 
  dplyr::as_tibble()
precip.handsjo <- read.csv2("data/tassasen/Rainfall_dygn_Handsjon.csv", skip = 9, header = TRUE,
                         stringsAsFactors = F) %>% 
  dplyr::rename(date = 3, prec.handsjo = 4) %>% 
  dplyr::mutate(prec.handsjo = as.numeric(prec.handsjo),
                date = lubridate::ymd(date, tz = "UTC")) %>% 
  dplyr::as_tibble()

# join prec, snow and temp
weather.tass <- temp.mean.ytterhogdal %>% 
  dplyr::full_join(precip.handsjo, by = c("date")) %>% 
  dplyr::left_join(precip.ratan, by = c("date")) %>%
  dplyr::left_join(snow.handsjo, by = c("date")) %>%
  dplyr::left_join(temp.min_max.ytterhogdal, by = c("date")) %>%
  dplyr::left_join(snow.ratan, by = c("date")) %>%
  dplyr::select(date, snow.ratan, snow.handsjo, prec.ratan, prec.handsjo,
                temp_min, temp_max, temp_avg) #%>% 
# dplyr::mutate(snow2 = if_else(is.na(snowh), snow, snowh))

# filter years and seasons of interest
weather.tass <- weather.tass %>% 
  dplyr::mutate(year = lubridate::year(date + lubridate::days(5*30)),
                month = lubridate::month(date)) %>% 
  dplyr::filter(month <= 4 | month >= 10, year >= 2008) %>% 
  dplyr::relocate(year, .after = date) %>% 
  dplyr::select(-month)

# check completness of datasets for each variable

# snow Ratan - many gaps, less frequent
ggplot(weather.tass, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = snow.ratan)) +
  labs(x = "Date", "Snow depth (mm)")

# snow Handsjo - almost complete, a few gaps
ggplot(weather.tass, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = snow.handsjo)) +
  labs(x = "Date", "Snow depth (mm)")

# precipitation Ratan - complete
ggplot(weather.tass, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = prec.ratan)) +
  labs(x = "Date", "Snow depth (mm)")

# precipitation Handsjo - complete
ggplot(weather.tass, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = prec.handsjo)) +
  labs(x = "Date", "Snow depth (mm)")

# temp_min, temp_max Ytterhogdal - complete
ggplot(weather.tass, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = temp_max)) +
  geom_line(aes(y = temp_min), col = "red") +
  labs(x = "Date", "Temperature (min, max)")

# temp_avg, Ytterhogdal - few gaps (eg. 2015)
ggplot(weather.tass, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = temp_avg)) +
  labs(x = "Date", "Snow depth (mm)")

# save
weather_tassasen_allstations <- weather.tass
saveRDS(weather_tassasen_allstations, "data/weather_tassasen_allstations.rds")
save(weather_tassasen_allstations, file = "data/weather_tassasen_allstations.RData")

# subset of stations
weather_tassasen <- weather.tass %>% 
  dplyr::select(-contains("ratan")) %>% 
  dplyr::rename(snow_depth = 3, prec = 4)

# save
saveRDS(weather_tassasen, "data/weather_tassasen.rds")
save(weather_tassasen, file = "data/weather_tassasen.RData")
