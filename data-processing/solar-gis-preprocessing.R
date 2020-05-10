library(raster)
library(tidyverse)
library(lubridate)

path <- "raw-data/United-Kingdom_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/monthly/"
files <- dir(path, pattern = ".tif$")

Brk_pvout <- stack()
for(i in 1:length(files)){
  Brk_pvout <- addLayer(Brk_pvout, raster(paste0(path, files[i])))
}
rm(files, path, i)

# longitude and latitude
source("data-processing/location-private.R")

# extract PV output
Dat_solar <- raster::extract(Brk_pvout, coords_EL) %>%
  as_tibble() %>%
  gather(key = "key", value = "kwh_kwp") %>%
  mutate(month = str_extract(key, "\\d+") %>% as.numeric()) %>%
  select(month, kwh_kwp)

# read in cloud % raster brick
Brk_cld <- brick("raw-data/cru_ts4.03.2011.2018.cld.dat.nc", varname = "cld")

# extract values and summarise 8-year-mean
Dat_solar <- raster::extract(Brk_cld, coords_EL) %>%
  as_tibble() %>%
  gather(key = "key", value = "cld_pc") %>%
  mutate(date = key %>%
           str_replace("X", "") %>%
           ymd(),
         month = month(date)) %>%
  group_by(month) %>%
  summarise(cld_pc_av = mean(cld_pc),
            cld_pc_sd = sd(cld_pc)) %>%
  right_join(Dat_solar, by = "month")

# write out dataset
write_rds(Dat_solar, path = "model-data/solar-gis-data-el.rds")
