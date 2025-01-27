#weathercan graphiques

#2025-01-26

library(tidyverse)
library(weathercan)

#download data------------------------

#vancouver stations
stations() %>% 
  filter(interval=="day") %>% 
  filter(str_detect(station_name,"VANCOUVER INT")) %>% view()

#889 and 51442 for data from international airport between 1937 and 2024

vancouver <- weather_dl(station_ids=c(889,51442),interval="day")

#MONTREAL DATA

stations() %>% 
  filter(interval=="day") %>% 
  filter(str_detect(station_name,"MONTREAL"))

#5415, 30165 and 51157 for montreal intl airport

montreal <- weather_dl(station_ids=c(5415,30165,51157),interval="day")

#Vancouver : number of freeze days by year---------------------

vancouver_mintemps <- 
  vancouver %>% 
  select(station_name,min_temp,date) %>% 
  group_by(date) %>% 
  summarise(min_temp = ifelse(all(is.na(min_temp)), NA,
                              min(min_temp, na.rm = TRUE))) %>% 
  select(date,min_temp) %>% 
  ungroup() %>% 
  distinct()

#gg_vancouver <- 
vancouver_mintemps %>% 
  mutate(year=year(date),
         freeze_day=if_else(min_temp<=0,1,0)) %>% 
  group_by(year) %>% 
  summarise(freeze_days=sum(freeze_day,na.rm=TRUE)) %>% 
  filter(year>=1955) %>% #to have the same range as the montreal graph 
  ggplot()+
  geom_col(aes(x=year,y=freeze_days)) +
  labs(title = "Vancouver: freezing days by year (1955 to 2024)",
       x = "Year",
       y = "Freezing days")

#ggsave(filename="output/vancouver.png",gg_vancouver,
#       dpi=300,width=6,height=4)

#Between 1955 and 1990, 11 winters where 60 or more days with freezing temperatures were recorded
#Since 1990, only one (1996)

#Montréal : max neige au sol par année-------------------------

#gg_montreal <- 
montreal %>% 
  select(station_name,date,snow_grnd) %>% 
  group_by(date) %>% 
  summarise(snow_on_ground = ifelse(all(is.na(snow_grnd)), NA,
                                    max(snow_grnd, na.rm = TRUE))) %>% 
  mutate(year=year(date)) %>% 
  group_by(year) %>% 
  summarise(max_snow_depth=ifelse(all(is.na(snow_on_ground)), NA,
                                  max(snow_on_ground, na.rm = TRUE))) %>% 
  filter(!is.na(max_snow_depth)) %>% 
  ggplot()+
  geom_col(aes(x=year,y=max_snow_depth))+
  labs(
    title = "Montréal : neige au sol maximale par année (1955 à 2024)",
    x = "Année",
    y = "Neige au sol (cm)"
  )

#ggsave(filename="output/montreal.png",gg_montreal,
#       dpi=300,width=6,height=4)

#Entre 1995 et 1990, 15 hivers dont la profondeur de la neige maximale à Montréal
#dépassait 50cm. Depuis 1990, ce n'est qu'une seule fois, en 2001, qu'on a enregistré 
#autant de neige au sol.

#annexe : viewing weather station location-----------------

library(mapview)

stations() %>% 
  filter(interval=="day") %>% 
  filter(str_detect(station_name,"MONTREAL")) %>% 
  select(station_name,start,end,lat,lon) %>% 
  filter(!is.na(lat),
         !is.na(lon)) %>% 
  st_as_sf(coords=c("lon","lat"),crs=4269) %>% mapview()
