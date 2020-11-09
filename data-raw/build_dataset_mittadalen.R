#' ---
#' title: 'Build dataset for winter conditions in Mittadalen sameby, Sweden'
#' author: Bernardo Niebuhr
#' ---

library(magrittr)
library(dplyr)
library(ggplot2)

# read data
snow.hagg <- read.csv2("data/mittadalen/Snodjup_Haggberget.csv", skip = 10, header = TRUE, stringsAsFactors = F) %>% 
  dplyr::rename(date = 1, snow.haggberget = 3) %>% 
  dplyr::mutate(date = lubridate::ymd(date, tz = "UTC"),
                snow.haggberget = 100*as.numeric(snow.haggberget)) %>% 
  dplyr::as_tibble()
snow.mysk <- read.csv2("data/mittadalen/Snodjup_Myskelasen.csv", skip = 10, header = TRUE, stringsAsFactors = F) %>% 
  dplyr::rename(date = 1, snow.myskelasen = 3) %>% 
  dplyr::mutate(date = lubridate::ymd(date, tz = "UTC"),
                snow.myskelasen = 100*as.numeric(snow.myskelasen)) %>% 
  dplyr::as_tibble()
snow.hede <- read.csv("data/mittadalen/Snodjup_Hedeviken.csv", header = TRUE) %>% 
  dplyr::rename(date = 1, snow.hedeviken = 3) %>% 
  dplyr::mutate(date = lubridate::ymd(date, tz = "UTC"),
                snow.hedeviken = snow.hedeviken*100) %>% 
  dplyr::as_tibble()
temp.min_max.drav <- read.csv2("data/mittadalen/Temperatur_max_min_dygn_Dravagen.csv", skip = 9, header = TRUE,
                               stringsAsFactors = F) %>%
  dplyr::rename(date = 3, temp_min = 4, temp_max = 6) %>%
  dplyr::mutate(date = lubridate::ymd(date, tz = "UTC"),
                temp_min = as.numeric(temp_min),
                temp_max = as.numeric(temp_max)) %>% 
  dplyr::as_tibble()
temp.mean.drav <- read.csv("data/mittadalen/Temperatur_dygn_Dravagen.csv", header = TRUE,
                            stringsAsFactors = F) %>%
  dplyr::rename(date = 3, temp_avg = 4) %>%
  dplyr::mutate(date = lubridate::ymd(date, tz = "UTC")) %>% 
  dplyr::as_tibble()
precip.drav <- read.csv2("data/mittadalen/Rainfall_dygn_Dravagen.csv", skip = 9, header = TRUE, 
                         stringsAsFactors = F) %>% 
  dplyr::rename(date = 3, prec.dravagen = 4) %>% 
  dplyr::mutate(prec.dravagen = as.numeric(prec.dravagen),
                date = lubridate::ymd(date, tz = "UTC")) %>% 
  dplyr::as_tibble()
precip.hede <- read.csv2("data/mittadalen/Rainfall_dygn_Hedeviken.csv", skip = 9, header = TRUE,
                         stringsAsFactors = F) %>% 
  dplyr::rename(date = 3, prec.hedeviken = 4) %>% 
  dplyr::mutate(prec.hedeviken = as.numeric(prec.hedeviken),
                date = lubridate::ymd(date, tz = "UTC")) %>% 
  dplyr::as_tibble()

# join prec, snow and temp
weather.mitt <- precip.drav %>% 
  dplyr::left_join(precip.hede, by = c("date")) %>% 
  dplyr::left_join(snow.hagg, by = c("date")) %>%
  dplyr::left_join(snow.hede, by = c("date")) %>%
  dplyr::left_join(snow.mysk, by = c("date")) %>%
  dplyr::left_join(temp.min_max.drav, by = c("date")) %>%
  dplyr::left_join(temp.mean.drav, by = c("date")) %>%
  dplyr::select(date, snow.haggberget, snow.hedeviken, snow.myskelasen, 
                prec.dravagen, prec.hedeviken, temp_min, temp_max, temp_avg) #%>% 
# dplyr::mutate(snow2 = if_else(is.na(snowh), snow, snowh))

# filter years and seasons of interest
weather.mitt <- weather.mitt %>% 
  dplyr::mutate(year = lubridate::year(date + lubridate::days(5*30)),
                month = lubridate::month(date)) %>% 
  dplyr::filter(month <= 4 | month >= 10, year >= 2008) %>% 
  dplyr::relocate(year, .after = date) %>% 
  dplyr::select(-month)

# check completness of datasets for each variable

# snow Haggberget - from 2012
ggplot(weather.mitt, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = snow.haggberget)) +
  labs(x = "Date", "Snow depth (mm)")

# snow Hedeviken - from 2009
ggplot(weather.mitt, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = snow.hedeviken)) +
  labs(x = "Date", "Snow depth (mm)")

# snow Myskelasen - complete
ggplot(weather.mitt, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = snow.myskelasen)) +
  labs(x = "Date", "Snow depth (mm)")

# precipitation Hedeviken - from 2008
ggplot(weather.mitt, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = prec.hedeviken)) +
  labs(x = "Date", "Snow depth (mm)")

# precipitation Dravagen - complete
ggplot(weather.mitt, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = prec.dravagen)) +
  labs(x = "Date", "Snow depth (mm)")

# temp_min, temp_max Dravagen - there is a gap in 2008
ggplot(weather.mitt, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = temp_max)) +
  geom_line(aes(y = temp_min), col = "red") +
  labs(x = "Date", "Snow depth (mm)")

# temp_avg, Dravagen - there is a gap in 2008
ggplot(weather.mitt, aes(date)) + 
  facet_wrap(.~year, scales = "free", ncol = 5)+
  geom_line(aes(y = temp_avg)) +
  labs(x = "Date", "Snow depth (mm)")

# save
weather_mittadalen_allstations <- weather.mitt
saveRDS(weather_mittadalen_allstations, "data/weather_mittadalen_allstations.rds")
save(weather_mittadalen_allstations, file = "data/weather_mittadalen_allstations.RData")

# subset of stations
weather_mittadalen <- weather.mitt %>% 
  dplyr::select(-contains("hedeviken"), -contains("hagg")) %>% 
  dplyr::rename(snow_depth = 3, prec = 4)

# save
saveRDS(weather_mittadalen, "data/weather_mittadalen.rds")
save(weather_mittadalen, file = "data/weather_mittadalen.RData")
