## Script to take the exported QGIS data and get the lat long and other site data cleaned and exported
## Schoodic Institute, 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(sf)



#------------------------------------------------#
####              Read in data                ####
#------------------------------------------------#

## Read in exported csv files
acmo <- tibble(read.csv("data/acadia_mountain_nr.csv"))
bemo <- tibble(read.csv("data/bernard_mountain_nr.csv"))
bmis <- tibble(read.csv("data/big_moose_island_r.csv"))
caba <- tibble(read.csv("data/cadillac_base_nr.csv"))
camo <- tibble(read.csv("data/cadillac_mountain_r.csv"))
otpo <- tibble(read.csv("data/otter_point_r.csv"))
pemo <- tibble(read.csv("data/penobscot_mountain_nr.csv"))
sche <- tibble(read.csv("data/schoodic_head_r.csv"))
scmo <- tibble(read.csv("data/schoodic_mountain_nr.csv"))
shha <- tibble(read.csv("data/ship_harbor_r.csv"))



#------------------------------------------------#
####             Clean and bind               ####
#------------------------------------------------#

### Cleaning for each site (n = 10)
## Acadia Mountain
acmof <- acmo %>% 
  mutate(id = paste0("acmo_nr_", id),
         longitude = (left + right)/2,
         latitude = (top + bottom)/2,
         refugia = "no",
         site = "acadia_mountain") %>% 
  select(id, site, refugia, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(3857) %>% 
  st_transform(4326) %>% 
  mutate(geom = gsub('[(c)°]', '', geometry)) %>% 
  separate(geom, into = c('longitude', 'latitude'), sep = '\\,') %>% 
  mutate(latitude = str_trim(latitude)) %>% 
  as_tibble() %>% 
  select(-geometry)

## Bernard Mountain
bemof <- bemo %>% 
  mutate(id = paste0("bemo_nr_", id),
         longitude = (left + right)/2,
         latitude = (top + bottom)/2,
         refugia = "no",
         site = "bernard_mountain") %>% 
  select(id, site, refugia, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(3857) %>% 
  st_transform(4326) %>% 
  mutate(geom = gsub('[(c)°]', '', geometry)) %>% 
  separate(geom, into = c('longitude', 'latitude'), sep = '\\,') %>% 
  mutate(latitude = str_trim(latitude)) %>% 
  as_tibble() %>% 
  select(-geometry)

## Big Moose Island
bmisf <- bmis %>% 
  mutate(id = paste0("bmis_r_", id),
         longitude = (left + right)/2,
         latitude = (top + bottom)/2,
         refugia = "yes",
         site = "big_moose_island") %>% 
  select(id, site, refugia, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(3857) %>% 
  st_transform(4326) %>% 
  mutate(geom = gsub('[(c)°]', '', geometry)) %>% 
  separate(geom, into = c('longitude', 'latitude'), sep = '\\,') %>% 
  mutate(latitude = str_trim(latitude)) %>% 
  as_tibble() %>% 
  select(-geometry)

## Cadillac Mountain base
cabaf <- caba %>% 
  mutate(id = paste0("caba_nr_", id),
         longitude = (left + right)/2,
         latitude = (top + bottom)/2,
         refugia = "no",
         site = "cadillac_base") %>% 
  select(id, site, refugia, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(3857) %>% 
  st_transform(4326) %>% 
  mutate(geom = gsub('[(c)°]', '', geometry)) %>% 
  separate(geom, into = c('longitude', 'latitude'), sep = '\\,') %>% 
  mutate(latitude = str_trim(latitude)) %>% 
  as_tibble() %>% 
  select(-geometry)

## Cadillac Mountain summit
camof <- camo %>% 
  mutate(id = paste0("camo_r_", id),
         longitude = (left + right)/2,
         latitude = (top + bottom)/2,
         refugia = "yes",
         site = "cadillac_mountain") %>% 
  select(id, site, refugia, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(3857) %>% 
  st_transform(4326) %>% 
  mutate(geom = gsub('[(c)°]', '', geometry)) %>% 
  separate(geom, into = c('longitude', 'latitude'), sep = '\\,') %>% 
  mutate(latitude = str_trim(latitude)) %>% 
  as_tibble() %>% 
  select(-geometry)

## Otter Point
otpof <- otpo %>% 
  mutate(id = paste0("otpo_r_", id),
         longitude = (left + right)/2,
         latitude = (top + bottom)/2,
         refugia = "yes",
         site = "otter_point") %>% 
  select(id, site, refugia, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(3857) %>% 
  st_transform(4326) %>% 
  mutate(geom = gsub('[(c)°]', '', geometry)) %>% 
  separate(geom, into = c('longitude', 'latitude'), sep = '\\,') %>% 
  mutate(latitude = str_trim(latitude)) %>% 
  as_tibble() %>% 
  select(-geometry)

## Penobscot and Sargent Mountain
pemof <- pemo %>% 
  mutate(id = paste0("pemo_nr_", id),
         longitude = (left + right)/2,
         latitude = (top + bottom)/2,
         refugia = "no",
         site = "penobscot_mountain") %>% 
  select(id, site, refugia, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(3857) %>% 
  st_transform(4326) %>% 
  mutate(geom = gsub('[(c)°]', '', geometry)) %>% 
  separate(geom, into = c('longitude', 'latitude'), sep = '\\,') %>% 
  mutate(latitude = str_trim(latitude)) %>% 
  as_tibble() %>% 
  select(-geometry)

## Schoodic Head
schef <- sche %>% 
  mutate(id = paste0("sche_r_", id),
         longitude = (left + right)/2,
         latitude = (top + bottom)/2,
         refugia = "yes",
         site = "schoodic_head") %>% 
  select(id, site, refugia, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(3857) %>% 
  st_transform(4326) %>% 
  mutate(geom = gsub('[(c)°]', '', geometry)) %>% 
  separate(geom, into = c('longitude', 'latitude'), sep = '\\,') %>% 
  mutate(latitude = str_trim(latitude)) %>% 
  as_tibble() %>% 
  select(-geometry)

## Schoodic Mountain
scmof <- scmo %>% 
  mutate(id = paste0("scmo_nr_", id),
         longitude = (left + right)/2,
         latitude = (top + bottom)/2,
         refugia = "no",
         site = "schoodic_mountain") %>% 
  select(id, site, refugia, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(3857) %>% 
  st_transform(4326) %>% 
  mutate(geom = gsub('[(c)°]', '', geometry)) %>% 
  separate(geom, into = c('longitude', 'latitude'), sep = '\\,') %>% 
  mutate(latitude = str_trim(latitude)) %>% 
  as_tibble() %>% 
  select(-geometry)

## Ship Harbor
shhaf <- shha %>% 
  mutate(id = paste0("shha_r_", id),
         longitude = (left + right)/2,
         latitude = (top + bottom)/2,
         refugia = "yes",
         site = "ship_harbor") %>% 
  select(id, site, refugia, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  sf::st_set_crs(3857) %>% 
  st_transform(4326) %>% 
  mutate(geom = gsub('[(c)°]', '', geometry)) %>% 
  separate(geom, into = c('longitude', 'latitude'), sep = '\\,') %>% 
  mutate(latitude = str_trim(latitude)) %>% 
  as_tibble() %>% 
  select(-geometry)


## Combine all site data for export
finaldat <- bind_rows(acmof, bemof, bmisf, cabaf, camof, otpof, pemof, schef, scmof, shhaf) %>% 
  mutate(site.code = str_remove(id, "\\_\\w*")) %>% 
  select(id, site, site.code, everything())
  
## Clean
sites <- finaldat %>% 
  select(site, site.code, refugia) %>% 
  distinct()
  
## Export final data
write.csv(test, "outputs/refugia_gridcell_data.csv", row.names = F)
write.csv(sites, "outputs/refugia_site_info.csv", row.names = F)
  
  
  