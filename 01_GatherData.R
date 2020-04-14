pacman::p_load(jsonlite, purrr, magrittr, dplyr, tidyr, readr)
library(sf)
library(googleway)

rm(list = ls())

## Get initial dataset containing all info available in the API for each train services
get_url <- function(id) {
  id <- gsub(" ", "%20", id)
  return(paste0("https://www.trafi.com/api/schedules/jakarta/schedule?scheduleId=", id, "&transportType=train"))
}

trains <- fromJSON("https://www.trafi.com/api/schedules/jakarta/all?transportType=train")[[1]] %>% unnest()

trains_jkt <- trains %>%
  mutate(api_url = map_chr(scheduleId, get_url),
         tanggal = Sys.Date(),
         info_rute = map(api_url, fromJSON),
         color = paste0("#", color)) %>%
  select(-transportNamePlural) %>%
  rename(moda = transportName, layanan_id = scheduleId, moda_id = transportId,
         nama_singkat = name, layanan = longName, warna = color, moda_icon = icon) %>%
  mutate(stasiun = map(info_rute, "stops"),
         rute = map(info_rute, "tracks"))

# Got the data
write_rds(trains_jkt, "data/rds/trains_jkt.rds")

## Create spatial data for stasiun
trains_jkt <- read_rds("data/rds/trains_jkt.rds")
stasiun <- trains_jkt %>%
  select(-info_rute, -rute) %>%
  unnest() %>%
  select(-areaName, -directionName) %>%
  rename(id_stasiun = id, nama_stasiun = name, lon = lng, 
         stasiun_icon = icon, peta_icon = mapIcon) %>%
  group_by(id_stasiun, nama_stasiun, lon, lat) %>%
  summarise(n_layanan = n_distinct(layanan_id),
            transit_id = list(unique(layanan_id)),
            transit = list(unique(nama_singkat))) %>%
  ungroup()

# Basic table..
write_rds(stasiun, "data/rds/stasiun.rds")

stasiun_pt <- stasiun %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_cast("POINT")
st_crs(stasiun_pt) <- 4326

st_write(stasiun_pt, "C:/Users/Tombayu Hidayat/Documents/Coding/Transportation/Map/gis/jakarta-trains/data/shp/stasiun.shp")
st_write(stasiun_pt, "C:/Users/Tombayu Hidayat/Documents/Coding/Transportation/Map/gis/jakarta-trains/data/geojson/stasiun.geojson")

## Create spatial data for the routes
trains_jkt <- read_rds("data/rds/trains_jkt.rds")
rute_pl <- trains_jkt %>%
  select(-info_rute, -stasiun) %>%
  unnest() %>%
  mutate(koordinat = map(shape, decode_pl)) %>%
  unnest(koordinat) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  group_by_at(vars(-stops, -geometry)) %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")
st_crs(rute_pl) <- 4326

st_write(rute_pl, "C:/Users/Tombayu Hidayat/Documents/Coding/Transportation/Map/gis/jakarta-trains/data/shp/rute.shp")
st_write(rute_pl, "C:/Users/Tombayu Hidayat/Documents/Coding/Transportation/Map/gis/jakarta-trains/data/geojson/rute.geojson")
