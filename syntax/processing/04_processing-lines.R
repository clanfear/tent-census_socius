# I can just mix all line data together in the same format so that there's 
# just one line file to load in later and work with.
# Only features here are trails, bridges, and streets which are buffered 40 feet.
library(tidyverse)
library(sf)
source("./syntax/project_functions.R")

# BRIDGES
# Limiting to bridges owned by the city or state that are over 100 feet long
# This gets rid of things we might not consider bridges---basically roads over
# a culvert. Also I sliced off the north segment of the Alaskan Way Viaduct
# that isn't there anymore.

bridges_raw <- st_read("./data/raw/data_seattle_gov/Bridges/Bridges.shp")
bridges <-
  bridges_raw |>
    mutate(length = units::set_units(st_length(geometry), "ft")) |> 
    filter(length >= units::set_units(100, "ft") & 
             BR_OWNER_N %in% c("Seattle Dept. of Transportation", "State of Washington") &
             !STATUS %in% "PLANNED" &
             !str_detect(UNITDESC_B, "BOAT RAMP|SKYBRIDGE|CULVERT|SEAWALL|ALASKAN WAY VIADUCT$") &
             !str_detect(BR_NAME, "BOAT RAMP|SKYBRIDGE|CULVERT|SEAWALL|ALASKAN WAY VIADUCT$|WASHINGTON STATE PIER|BATTERY STREET TUNNEL|LAKEWAY TUNNEL") &
             !str_detect(BR_NAME, "PED") &
             !str_detect(BR_FACILIT, "PED")) |>
  bind_rows(bridges_raw |>
              filter(str_detect(UNITDESC_B, "ALASKAN WAY VIADUCT$") |
                       str_detect(BR_NAME, "ALASKAN WAY VIADUCT$")) |>
              st_cast(to = 'LINESTRING',warn = FALSE) |> 
              slice(2L) |>
              st_cast(to = 'MULTILINESTRING') |>
              mutate(length = units::set_units(st_length(geometry), "ft"))) |>
  transmute(name = BR_NAME, 
            type = "Bridge", 
            address = UNITDESC_S, lat = NA_real_, long = NA_real_, geometry)

# STREETS
# Full street grid. FOcusing here on a simple categorization of streets.
streets <- st_read("./data/raw/data_seattle_gov/Seattle_Streets/Seattle_Streets.shp") |>
  filter(!is.na(ARTDESCRIP)) |>
  transmute(
    name = STNAME_ORD, 
    type = case_when(
      ARTDESCRIP %in% c("Principal Arterial", "Minor Arterial", 
                        "County Arterial", "Collector Arterial")     ~ "Arterial",
      ARTDESCRIP %in% c("Not Designated")                            ~ "Collector",
      ARTDESCRIP %in% c("Interstate/Freeway", "State Route/Freeway") ~ "Highway/Freeway",
      TRUE ~ NA_character_
    ), address = UNITDESC,  lat = NA_real_, long = NA_real_, geometry
  )

# TRAILS
# This is basically major bike and ped trails
trails <- st_read("./data/raw/data_seattle_gov/Multi-use_Trails_(Seattle_Only)/Multi-use_Trails_(Seattle_Only).shp") |> 
  mutate(length = units::set_units(st_length(geometry), "ft")) |> 
  filter(length >= units::set_units(100, "ft") & ORD_STRE_1 == "TRL") |>
  transmute(
    name = ORD_STNAME, 
    type = "Trail", address = NA_character_,  lat = NA_real_, long = NA_real_, geometry
  )

# SIDEWALKS
# Not using sidewalks at moment
# sidewalks <- st_read("./data/data_seattle_gov/Sidewalks/Sidewalks.shp") |> st_drop_geometry()

# Merge all line features together into a single set for ease of use
line_features <- bind_rows(bridges, streets, trails)
save(line_features, file = "./data/derived/line_features.RData")

# Line features for joining to point data; 40 foot buffers
line_features_for_join <- line_features |> 
  select(type, geometry) |> 
  st_transform(3689) |> 
  group_by(type) |>
  summarize(geometry = st_union(geometry)) |>
  st_buffer(dist = units::set_units(40, "ft")) |>    # Adding a 40 foot buffer to get anything at all on the street
  st_transform(4326) |> 
  make_dums("type") %>%
  setNames(., snakecase::to_snake_case(names(.)))
save(line_features_for_join, file = "./data/derived/line_features_for_join.RData")
