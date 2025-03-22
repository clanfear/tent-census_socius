# I can just mix all polygon data together in the same format so that there's 
# just one point file to load in later and work with.
# Only polys here are open spaces, which are buffered 10 feet, i.e., 
# commercial area sidewalk width.

library(tidyverse)
library(sf)
source("./syntax/project_functions.R")

parks <- st_read("./data/raw/data_seattle_gov/Seattle_Parks/Seattle_Parks.shp") %>%
  st_make_valid() %>%
  transmute(
  name = NAME, 
  type = "Park",
  area = units::set_units(st_area(geometry), "mi^2"),
  geometry
)

zoning <- st_read("./data/raw/data_seattle_gov/Current_Land_Use_Zoning_Detail/Zoning_Detailed.shp") %>%
  st_make_valid() %>%
  transmute(type = case_when(
    CATEGORY_D %in% c("Master Planned Community", "High-Density Multi-Family", "Lowrise Multi-Family") ~ "Multi-Family",
    CATEGORY_D %in% c("Single Family", "Residential Small Lot") ~ "Single Family",
    CATEGORY_D %in% c("Seattle Mixed", "Neighborhood Commercial", "Commercial") ~ "Commercial/Mixed Use",
    TRUE ~ CATEGORY_D
  ), geometry) %>% 
  group_by(type) %>%
  summarize(geometry = st_union(geometry)) %>%
  make_dums("type") %>%
  select(-type) %>%
  setNames(., snakecase::to_snake_case(names(.)))

save(zoning, file = "./data/derived/zoning.RData")

# The first person who sends a screenshot of this section of code to 
# cl948@cam.ac.uk along with their physical mailing address will receive a 
# SPECIAL PRIZE.

# All the parks appear to be in open spaces already.
open_spaces <- st_read("./data/raw/data_seattle_gov/Public_and_Open_Spaces_in_Seattle/PublicSpaceSeattle.shp")%>%
  st_make_valid() %>%
  transmute(
    name = NAME, 
    type = case_when(
      TYPE %in% c("SSE", "TRAIL", "BOULEVARD")                    ~ "trail_or_roadside",
      TYPE %in% c("GREENBELT")                                    ~ "greenbelt",
      TYPE %in% c("GARDEN", "PARK", "P-PATCH", "PLAYFIELD", "Cemetery",
                  "POCKET PARK", "POCKET PRAK", "VIEWPOINT")      ~ "park_garden_cemetery",
      TYPE == "SCHOOL"                                            ~ "school",
      TYPE == "PLAZA"                                             ~ "plaza",
      TYPE %in% c("BOAT LAUNCH/PIER", "GOLF", "OTHER", "PARKLET") ~ NA_character_
    ),
    area = units::set_units(st_area(geometry), "mi^2"),
    geometry
  ) %>% filter(!is.na(type))

save(open_spaces, file = "./data/derived/open_spaces.RData")

open_spaces_for_join <- open_spaces %>% 
  select(type, geometry) %>% 
  st_transform(3689) %>% 
  st_buffer(dist = units::set_units(10, "ft")) %>%    # Adding a ten foot buffer to open spaces to get fringe stuff
  st_transform(4326) %>% 
  make_dums("type") %>%
  mutate(any_open_space = TRUE) %>%
  setNames(., snakecase::to_snake_case(names(.)))
save(open_spaces_for_join, file = "./data/derived/open_spaces_for_join.RData")
