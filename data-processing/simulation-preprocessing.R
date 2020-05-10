library(tidyverse)
library(lubridate)

# build 1-year, hourly simulation dataset
Dat_sim <- tibble(dttm = seq(from = ymd_hms("2021-01-01 00:00:00"),
                             to = ymd_hms("2021-12-31 23:00:00"),
                             by = as.difftime(hours(1))),
                  month = month(dttm),
                  yday = yday(dttm),
                  hour = hour(dttm))

# add in solar gis data
Dat_sim <- Dat_sim %>%
  left_join(read_rds("model-data/solar-gis-data-el.rds"), by = "month") %>%
  mutate(cld_pc_av = cld_pc_av / 100,
         cld_pc_sd = cld_pc_sd / 100)

# location data
source("data-processing/location-private.R")
Dat_sim <- Dat_sim %>%
  mutate(lon = coords_EL$x,
         lat = coords_EL$y)

# add in solar angle
library(insol)

Dat_sim <- Dat_sim %>%
  mutate(time_zone = 0,
         sunrise = pmap_dbl(list(lat, lon, yday, time_zone), function(a, b, c, d){
           return(daylength(a, b, c, d)[1])
         }),
         sunset = pmap_dbl(list(lat, lon, yday, time_zone), function(a, b, c, d){
           return(daylength(a, b, c, d)[2])
         })) %>%
  select(-time_zone)

library(oce)

Dat_sim <- Dat_sim %>%
  mutate(solar_alt = pmap_dbl(list(dttm, lon, lat), function(a, b, c){
    return(sunAngle(a, b, c)$altitude)
  }))

# write out rds
write_rds(Dat_sim, "model-data/simulation-base-data.rds")
