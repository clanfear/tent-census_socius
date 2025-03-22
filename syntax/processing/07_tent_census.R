# This file processes the spreadsheets from the Summer 2019 tent census, 
# which searched the entire city of Seattle, and the autumn 2019 and summer 2020
# resamples that covered only areas of the city with significant numbers of 
# tents in the census.

# CENSUS: SUMMER 2019
# The census ran from April 4th to August 23rd, 2019 (141 days), according to timestamps.

library(tidyverse)
library(sf)
library(lubridate)
source("./syntax/project_functions.R")

# Raw data need a lot of manual processing to get counts, fix some locations, etc.
# The mutate() calls could all be collapsed into even just one, but I've kept 
# them separate here just for legibility since many variables are modified
# multiple times and it is nice to be able to run it partially and see what each
# step does.

tent_census_summer_2019 <- readxl::read_excel("./data/raw/Tent Census_April-August2019.xlsx") |>
  mutate(coordinate = str_extract(URL, "[0-9\\.]*,-[0-9\\.]*$")) |> 
  mutate(coordinate = case_when(
    Title == "4799-4701 9th Ave NW" ~ "47.663364,-122.370718", 
    URL == "https://www.google.com/maps/place/Seattle/data=!4m2!3m1!1s0x54906a961df6ee21:0x8119b0b7d5eea108" ~ "47.595891,-122.3219297",
    URL == "https://www.google.com/maps/place/Elliott+Bay+Trail/data=!4m2!3m1!1s0x54906ab27ec1f023:0x51ba1d0307faef5e" ~ "47.6077857,-122.3443708",
    TRUE ~ coordinate)) |>
  separate(coordinate, c("latitude", "longitude"), ",", remove=FALSE, convert=TRUE) |>
  mutate(note_upper = str_to_upper(Note)) |>
  mutate(note_clean = str_remove(note_upper, "^.*//")) |>
  mutate(note_clean = str_remove_all(note_clean, 
    "(ABANDONED |TREE |NATURAL |WOODEN |LARGE |ORANGE |SMALL NORTHFACE |SHARK-LOOKING |GREEN |DILAPITATED |LITTLE |BROWN |YELLOW |BIG |BLUE |GREY |HUGE |SM |LG |CARDBOARD |THREE PART )")) %>%
  mutate(n_tents = ifelse(str_detect(note_clean, "TENT"), 
                          str_extract(note_clean, "[0-9][0-9]? TENT(S)?"), 0)) |>
  mutate(n_tents = as.numeric(ifelse(is.na(n_tents), 0, str_extract(n_tents, "\\d+")))) |>
  mutate(n_tents = case_when(
    str_detect(Note, "Camp, cannot see in 6/20 11:21") ~ 5, 
    str_detect(Note, "Bottom of 8 tents 6/13 10:33 am") ~ 4,
    str_detect(Note, "At least one t one s 7/15 2:30") ~ 1,
    TRUE ~ n_tents)) |>
  mutate(n_structures = ifelse(str_detect(note_clean, "STRUCTURE"), 
                               str_extract(note_clean, "[0-9][0-9]? STRUCTURE(S)?"), 0)) %>%
  mutate(n_structures = as.numeric(ifelse(is.na(n_structures), 0, str_extract(n_structures, "\\d+")))) |>
  mutate(n_structures = case_when(
    str_detect(Note, "At least one t one s 7/15 2:30") ~ 1,
    str_detect(note_upper, "6 TENTS 3 STRUCTURES 6/24 10:59") ~ 3,
    str_detect(note_upper, "AT LEAST 4 STRUCTURES 6/25 AFTERNOON") ~ 5,
    str_detect(note_upper, "1 HUGE STRUCTURE 5/13 11:00 AM") ~ 5,
    TRUE ~ n_structures
  )) %>%
  mutate(n_dwellings = n_tents + n_structures) %>%
  distinct(latitude, longitude, n_dwellings, Note, n_tents, n_structures) %>%
  rename(note = Note) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  rename(long = longitude, lat = latitude) %>%
  mutate(sample_type = "census", wave = 1L)

# RESAMPLE 1: AUTUMN 2019
# Resamples are in a cleaner format, so little needs to be done.

tent_census_autumn_2019 <-  
  readxl::read_excel("./data/raw/Master TC_TR Database 11 28 2019 Tent Census Originals Database Full Form.xlsx", 
                     sheet = "TR1 Total") %>%
  rename(note        = Description, 
         latitude    = `LAT DD`, 
         longitude   = `LONG DD`, 
         n_dwellings = Size) %>%
  distinct(latitude, longitude, n_dwellings, note) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  rename(long = longitude, lat = latitude)  %>%
  mutate(sample_type = "resample", wave = 2L)

# RESAMPLE 2: SUMMER 2020

tent_census_summer_2020 <-  
  readxl::read_excel("./data/raw/Master TC_TR Database 11 28 2019 Tent Census Originals Database Full Form.xlsx", 
                     sheet = "TR2 Total") %>%
  rename(note        = Description, 
         latitude    = `LAT DD`, 
         longitude   = `LONG DD`, 
         n_dwellings = `Size of encampment`) %>%
  mutate(n_dwellings = ifelse(is.na(n_dwellings), 1, n_dwellings)) %>%
  distinct(latitude, longitude, n_dwellings, note) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  rename(long = longitude, lat = latitude)  %>%
  mutate(sample_type = "resample", wave = 3L)

# Merge
tent_census <- bind_rows(tent_census_summer_2019 |>
                           select(-n_tents, -n_structures), 
                         tent_census_autumn_2019, 
                         tent_census_summer_2020) %>%
  mutate(id = row_number())
save(tent_census, file = "./data/derived/tent_census.RData")
