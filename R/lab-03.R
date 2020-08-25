# SPDS
library(tidyverse)
library(sf)
library(units)

# Data
library(USAboundaries)
library(rnaturalearth)

# Visualization
library(gghighlight)
library(ggrepel)
library(knitr)

library(dplyr)

eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'



remotes::install_github("ropensci/USAboundaries", force=TRUE)
remotes::install_github("ropensci/USAboundariesData", force = TRUE)

remotes::install_github("ropenscilabs/rnaturalearthdata")


countries = rnaturalearth::countries110 %>%
  st_as_sf(coords=c("lng", "lat"), crs = 4326) %>%
  filter(admin %in% c("Canada", "United States of America", "Mexico")) %>%
  st_transform(eqdc)

USAboundaries::us_states(resolution = "low")
us <- us_states()

CONUS <- us %>%
  filter(!(name %in% c('Puerto Rico', 'Alaska', 'Hawaii'))) %>%
  st_as_sf(coords = c("lng","lat"), crs = 4326) %>%
  st_transform(eqdc)

cities = read_csv("~/Documents/github/geog-176A-labs/data/uscities.csv") %>%
  st_as_sf(coords = c("lng","lat"), crs = 4326) %>%
  filter(!(state_name %in% c('Puerto Rico', 'Alaska', 'Hawaii'))) %>%
  st_transform(eqdc)

#question 2

#2.1

eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'


usadistance = USAboundaries::us_states() %>%
  st_union() %>%
  st_cast("MULTILINESTRING") %>%
  st_transform(eqdc)

dist_to_us = cities %>%
  mutate(dist = st_distance(., usadistance),
         dist = set_units(dist, "km")) %>%
  select(city, dist, state_name)

cities %>%
  mutate(dist = st_distance(., usadistance),
         dist = set_units(dist, "km"),
         dist = drop_units(dist)) %>%
  select(city, dist, state_name) %>%
  slice_max(dist, n=5) ->
  far5

tablefar5 = far5 %>%
  st_drop_geometry()

knitr::kable(tablefar5,
             caption = "Five Cities Farthest From a US National Border",
             col.names = c("City", "Distance", "State_name"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = 14)


#2.2

dist2state = st_cast(CONUS, 'MULTILINESTRING') %>%
  select(geometry) %>%
  st_combine() %>%
  st_transform(eqdc)

distances = cities %>%
  mutate(dist = st_distance(., dist2state),
         dist = set_units(dist, "km"),
         dist = drop_units(dist))

cities %>%
  mutate(dist = st_distance(., dist2state),
         dist = set_units(dist, "km")) %>%
  select(city, dist, state_name) %>%
  slice_max(dist, n=5) ->
  far5fromborder

knitr::kable(st_drop_geometry(far5fromborder),
             caption = "Five Cities Farthest From a US State Boundary",
             col.names = c("City", "Distance", "State_name"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = 14)

#2.3
mex = countries %>%
  filter(admin %in% "Mexico")

dist_mex_all = cities %>%
  mutate(dist = st_distance(., mex),
         dist = set_units(dist, "km"))

dist_mex = dist_mex_all%>%
  select(city, dist, state_name)  %>%
  slice_max(dist, n= 5)

knitr::kable(st_drop_geometry(dist_mex),
             caption = "5 Cities Farthest from Mexican Border",
             col.names = c("City", "Distance", "State_name"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = 14)

#2.4
canada = countries %>%
  filter(admin %in% "Canada")
dist_canada_all = cities %>%
  mutate(dist = st_distance(cities, canada),
         dist = set_units(dist, "km")) %>%
  select(city, dist, state_name)

dist_canada = dist_canada_all%>%
  slice_max(dist, n=5)

knitr::kable(st_drop_geometry(dist_canada),
             caption = "5 Cities Farthest from Canadian Border",
             col.names = c("City", "Distance", "State_name"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = 14)

#question 3

#3.1
library(ggplot2)
library(ggrepel)
library(gghighlight)

largestUSAcities = cities %>%
  slice_max(population, n = 10)

ggplot()+
  geom_sf(data = countries)+
  geom_sf(data = usadistance, size = 2)+
  geom_sf(data = CONUS, lty = "dotted")+
  geom_sf(data = largestUSAcities, color = "purple", size = 0.7)+
  ggrepel::geom_label_repel(data = largestUSAcities, aes(label= city, geometry = geometry),
                            stat = "sf_coordinates", size = 1.75)

#3.2
library(ggplot2)

ggplot()+
  geom_sf(data = far5, color = "red")+
  geom_sf(data = cities)+
  geom_sf(data = dist_to_us, aes(col = as.numeric(dist)), size = .1) +
  scale_color_gradient(low = "light blue", high = "purple")+
  theme_linedraw()+
  ggrepel::geom_label_repel(data = far5,
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 3)+
  ggthemes::theme_map()+
  ggplot2::theme_linedraw()

#3.3

ggplot()+
  geom_sf(data = far5fromborder, color = "blue")+
  geom_sf(data = dist2state)+
  geom_sf(data = distances, aes(col = as.numeric(dist)), size = .1) +
  scale_color_gradient(low = "lightblue", high = "purple")+
  ggrepel::geom_label_repel(data = far5fromborder,
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 3)+
  ggthemes::theme_map()+
  ggplot2::theme_linedraw()


#3.4
dist_mex_can = distances %>%
  mutate(equidistant <- abs(dist_canada_all$dist-dist_mex_all$dist))

