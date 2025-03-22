library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
source("./syntax/project_functions.R")

# Tracts
if(!file.exists("./data/derived/seattle_tract_boundaries.RData")){
  seattle_tract_boundaries <- tracts("WA", county = "King", class="sf", year = 2010) |>
    select(tract = GEOID10, geometry) |>
    filter(as.numeric(str_sub(tract, -5, -1)) < 13000 | tract == 53033026500) |>
    # filter(GEOID != 53033005302) |> # University
    st_transform(4326)
  
  save(seattle_tract_boundaries, file = "./data/derived/seattle_tract_boundaries.RData")
} else {
  load("./data/derived/seattle_tract_boundaries.RData")
}
# Blockgroups
if(!file.exists("./data/derived/seattle_bg_boundaries.RData")){
  seattle_bg_boundaries <- block_groups("WA", county = "King", class="sf", year = 2010) |> 
    select(blockgroup = GEOID10, geometry) |> 
    filter(as.numeric(str_sub(blockgroup, -6, -1)) < 130000 | str_detect(blockgroup, "530330265001")) |>
    # filter(str_sub(GEOID, 1, -2) != "53033005302") |> # University
    st_transform(4326)

  save(seattle_bg_boundaries, file = "./data/derived/seattle_bg_boundaries.RData")
} else {
  load("./data/derived/seattle_bg_boundaries.RData")
}
# Blocks
if(!file.exists("./data/derived/seattle_block_boundaries.RData")){
  seattle_block_boundaries <- blocks("WA", county = "King", class="sf", year = 2010) |>
    filter(as.numeric(TRACTCE10) < 13000 | str_detect(GEOID10, "530330265001")) |>
    filter(ALAND10 > 0) |>
    select(block = GEOID10, geometry) |>
    st_transform(4326) |>
    filter(block %!in% c(530330046001028, 530330108001002)) # Two uninhabited islands.

  save(seattle_block_boundaries, file = "./data/derived/seattle_block_boundaries.RData")
} else {
  load("./data/derived/seattle_block_boundaries.RData")
}
# Dealing with water bodies
# I used BGs here for a reason that I forget. I think there's one BG in city limits
# but the other BGs in that tract aren't.
seattle_withwater <- seattle_bg_boundaries |>
  st_union() |> st_combine()
save(seattle_withwater, file = "./data/derived/seattle_withwater.RData")

# Seattle with a 200m buffer around it, used for filtering and maps
seattle_big_buffer <- seattle_withwater %>% st_as_sf() %>%
  st_transform(3689) %>% 
  st_buffer(dist = units::set_units(200, "meters")) %>%
  st_transform(4326) %>% st_geometry()
save(seattle_big_buffer, file = "./data/derived/seattle_big_buffer.RData")

kc_water <- area_water("WA", county = "King", class="sf") |>
  st_transform(4326) 

king_county <- counties("WA") |>
  filter(NAME == "King") 

king_county_nowater <- king_county |> 
  st_union() |> 
  st_combine() |> 
  st_transform(4326) |>
  st_difference(kc_water |>
                  arrange(desc(AWATER)) |>
                  slice_head(n=25) |>
                  filter(!str_detect(FULLNAME, "Riv")) |>
                  st_union() |> 
                  st_combine())
save(king_county_nowater, file = "./data/derived/king_county_nowater.RData")

kc_water <- kc_water |>
  st_union() |> 
  st_combine()
save(kc_water, file = "./data/derived/kc_water.RData")

seattle_nowater <- seattle_withwater |>
  st_difference(kc_water)

save(seattle_nowater, file = "./data/derived/seattle_nowater.RData")


