#packages

library(knitr)
library(raster)
library(tidyverse)
library(getlandsat)
library(sf)
library(mapview)

aoi = read_csv('~/Documents/github/geog-176A-labs/data/uscities.csv') %>%
  filter(city == 'Palo') %>%
  st_as_sf(coords = c('lng','lat'), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc()

palo_tf = st_transform(aoi, 4326) %>%
  st_bbox()

leaflet(palo_tf)

landsat = getlandsat::lsat_scenes()
land = landsat %>%
  filter(min_lat <= palo_tf$ymin,
         max_lat >= palo_tf$ymax,
         min_lon <= palo_tf$xmin,
         max_lon >= palo_tf$xmax,
         as.Date(acquisitionDate) == as.Date('2016-09-26'))


write.csv(land, file = "~/Documents/github/geog-176A-labs/data/palo-flood-scene.csv")


#Step 2

flood_scene = read_csv('~/Documents/github/geog-176A-labs/data/palo-flood-scene.csv')


files <- lsat_scene_files(flood_scene$download_url)%>%
  filter(grepl(paste0('B', 1:6, ".TIF$", collapse = '|'), file)) %>%
  arrange(file) %>%
  pull(file)

#Step 3
st = sapply(files, lsat_image)

b = stack(st) %>% setNames(paste0('band', 1:6))
area(b)

#Step 4
crop_ = aoi %>%
  st_as_sf() %>%
  st_transform(crs(b))

bCrop = crop(b, crop_)
area(bCrop)

