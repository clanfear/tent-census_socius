# I can just mix all point data together in the same format so that there's 
# just one point file to load in later and work with.

library(tidyverse)
library(tidygeocoder)
library(sf)
library(janitor)
library(readxl)
source("./syntax/project_functions.R")
load("./data/derived/seattle_big_buffer.RData")

if(!file.exists("./data/derived/city_properties.RData")){
  # City property areas; mostly libraries
  pma_names <-c(
    "St Martin De Porres Shelter- Lease",
    "Central Youth and Family Services",
    "Chief Seattle Club- Covenant & Esmt")
  # Filter and geocode using Arc
  city_properties <- read_csv("./data/raw/data_seattle_gov/city_property_areas_2018-12-20.csv") |>
    filter(
      (USE_CLASS == "Library/Community/Cultural Facilities" & USE=="Library") |
      PMA_NAME %in% pma_names
    ) |>
    transmute(name    = PMA_NAME, 
              type    = ifelse(USE == "Human Services Facility", "Shelter", USE), 
              address = paste0(ADDRESS, ", Seattle, WA")) |>
    filter(type != "Shelter") |>
    geocode(address = address, method = "arcgis") |>
    st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
  save(city_properties, file = "./data/derived/city_properties.RData")
} else {
  load("./data/derived/city_properties.RData")
}

# Mental Health and Substance Use Providers
mhsu_providers <- read_csv("./data/raw/data_seattle_gov/kc_mental_health_substance_use_2020-07-07.csv") |> 
  filter(str_detect(Locations, "Seattle") & Adults) |>
  transmute(name    = Provider, 
            type    = "Mental Health or Substance Use",
            address = str_remove(str_replace_all(Address, "\n", " "), " \\(.*$"),
            lat     = as.numeric(str_extract(Address, "[^\\()]\\d*\\.\\d*")),
            long    = as.numeric(str_extract(Address, "-\\d*\\.\\d*.*?(?=\\))"))) |>
  st_as_sf(coords   = c("long", "lat"), crs = 4326, remove = FALSE)

# Police stations
police <- st_read("./data/raw/data_seattle_gov/Police_Stations/Police_Stations.shp") |>
  transmute(name    = NAME, 
            address = ADDR, 
            type    = "Police Station", 
            lat     = st_coordinates(geometry)[,2], 
            long    = st_coordinates(geometry)[,1], 
            geometry)

# Fire stations
fire <- st_read("./data/raw/data_seattle_gov/Fire_Stations/Fire_Stations.shp")  |>
  filter(STNID != "HMC") |>
  transmute(name = STNID, 
            address = ADDRESS, 
            type = "Fire Station", 
            lat = st_coordinates(geometry)[,2], 
            long = st_coordinates(geometry)[,1], 
            geometry)

# Schools and services, which requires geocoding
if(!file.exists("./data/derived/schools_and_services.RData")){
schools_and_services <- map_dfr(c("Private_Schools", "Shelters", "Hygiene", "Housing_Asst", "Food_Asst", "Drug&Alcohol"), 
        ~ read_excel("./data/raw/SeattleServicesSchools.xlsx", 
                             sheet = .x, 
                             range = cell_cols("A:C"))) |>
  bind_rows(
    bind_rows(
      read_excel("./data/raw/SeattleSchools.xlsx") |> mutate(type = "Public School"),
      read_excel("./data/raw/DrugStores.xlsx") |> mutate(type = "Drug Store")
      ) |>
              setNames(., str_to_lower(names(.))) 
    ) |>
  filter(!is.na(address) & !is.na(name)) |>
  mutate(address = str_replace(address, " Seattle, WA", ", Seattle, WA")) |>
  geocode(address = address, method = "cascade", cascade_order = c("census", "arcgis")) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
save(schools_and_services, file = "./data/derived/schools_and_services.RData")

} else {
  load("./data/derived/schools_and_services.RData")
}


# Grocery stores
food_inspections <- read_csv("./data/raw/Food_Establishment_Inspection_Data_20241217.csv", guess_max = 10000)
grocery_stores <- food_inspections |> 
  clean_names() |> 
  mutate(inspection_date = mdy(inspection_date)) |>
  filter(inspection_date %within% interval(ymd("2019-01-01"), ymd("2020-01-01"))) |>
  filter(description == "Grocery Store-no seating - Risk Category I" | business_id == "PR0010498") |>
  filter(!str_detect(name, "CANTEEN VENDING|AVANTI MARKETS|FUEL CENTER")) |>
  distinct(name, address, latitude, longitude) |>
  mutate(type = case_when(
    str_detect(str_to_upper(name), "KEN'S MARKET|ASIAN FAMILY MARKET|WINCO|SAFEWAY|WALMART|UWAJIMAYA|QFC|PCC|FRED MEYER|DOLLAR TREE|COSTCO|BALLARD MARKET|ALBERTSONS") ~ 
      "Grocery Store",
    TRUE ~ NA
  )) |> 
  filter(!is.na(type)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  st_filter(seattle_big_buffer) |> # Filter to roughly within city limits
  select(name, address, type, geometry, lat = latitude, long = longitude)

# Merge

point_features <- bind_rows(city_properties, mhsu_providers, police, fire, schools_and_services, grocery_stores)
save(point_features, file = "./data/derived/point_features.RData")

# Point features recategorized for the maps
point_features_for_map <- point_features |>
  mutate(category = case_when(
    type %in% c("Private Schools", "Public School") ~ "School",
    type %in% c("Drug and Alcohol Services", "Mental Health or Substance Use") ~ "Drug, Alcohol, Mental Health",
    type %in% c("Food Assistance", "Housing Assistance", "Hygiene Services", "Shelters") ~ "Food, Housing, Hygiene, Shelters",
    type == "Drug Store" ~ "Drug Store",
    type == "Grocery Store" ~ "Grocery Store",
    type %in% c("Police Station", "Fire Station") ~ "Police or Fire",
    type == "Library" ~ "Library"
  ), .after = "type")
save(point_features_for_map, file = "./data/derived/point_features_for_map.RData")

# Point features rearranged for joining
point_features_for_join <- point_features |>
  select(type, geometry) |>
  st_transform(3689) |>
  mutate(buffer_size = ifelse(type %in% c("Bench", "Table"), 20, 200)) |>
  mutate(geometry = st_buffer(geometry, dist = units::set_units(buffer_size, "ft"))) |>
  select(-buffer_size) |>
  st_transform(4326) |> 
  make_dums("type") %>%
  setNames(., snakecase::to_snake_case(names(.))) |>
  mutate(category = case_when(
    type %in% c("Private Schools", "Public School") ~ "School",
    type %in% c("Drug and Alcohol Services", "Mental Health or Substance Use") ~ "Drug, Alcohol, Mental Health",
    type %in% c("Food Assistance", "Housing Assistance", "Hygiene Services", "Shelters") ~ "Food, Housing, Hygiene, Shelters",
    type == "Drug Store" ~ "Drug Store",
    type == "Grocery Store" ~ "Grocery Store",
    type %in% c("Police Station", "Fire Station") ~ "Police or Fire",
    type == "Library" ~ "Library"
  ), .after = "type")

save(point_features_for_join, file = "./data/derived/point_features_for_join.RData")
