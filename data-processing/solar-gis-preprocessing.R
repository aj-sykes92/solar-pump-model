library(raster)
library(tidyverse)

path <- "model-data/United-Kingdom_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/monthly/"
files <- dir(path, pattern = ".tif$")

Brk_pvout <- stack()
for(i in 1:length(files)){
  Brk_pvout <- addLayer(Brk_pvout, raster(paste0(path, files[i])))
}
rm(files, path, i)

# longitude and latitude for Easter Lalathen
coords_EL <- tibble(x = -3.050532, y = 56.227046)

# extract PV output for Easter Lalathen
Dat_pvout <- raster::extract(Brk_pvout, coords_EL) %>%
  as_tibble() %>%
  gather(key = "key", value = "kwh_kwp") %>%
  mutate(month = str_extract(key, "\\d+") %>% as.numeric()) %>%
  select(month, kwh_kwp)

rm(Brk_pvout)

# write out pvout dataset
write_rds(Dat_pvout, path = "model-data/kwh-per-kwp-easter-lalathen.rds")
