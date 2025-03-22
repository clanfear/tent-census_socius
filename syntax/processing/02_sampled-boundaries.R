# Getting correct sampled areas
library(tidyverse)
library(sf)
source("./syntax/project_functions.R")
load("./data/derived/seattle_withwater.RData")
load("./data/derived/seattle_nowater.RData")

# Okay so no shapefiles exist for what they sample. They sampled these:
# https://www.seattle.gov/neighborhoods/programs-and-services/neighborhood-planning

# From Karen:
# Areas Resampled:
# - All of I-5
# - Ballard (highlighted, but not the Crown Hill highlighted section)
# - Fremont (highlighted)
# - Interbay (Ballard side - above the canal)
# - Queen Anne (highlighted uptown, as indicated on the city planning map, not the other highlighted section)
# - Georgetown
# - Commercial Core
# - Chinatown & International District
# - Pioneer Square
#
# Andria also told me:
#   
#   We did continue to use the city planning maps for the resamples. When we say
#   we covered the highlighted part of a map for the resamples, we mean we
#   covered the darker section on the map. For some maps, the entire section is
#   highlighted so we did not say highlighted for those. These were the specific
#   areas we covered for the resamples. They are not specifically indicated on
#   the physical map in any specific way because we used that primarily during
#   the full census.

exclusion <- read_sf("./data/raw/sample_boundaries/exclusion.shp") %>%
  st_make_valid() %>%
  st_transform(4326)
sample_boundaries <- read_sf("./data/raw/sample_boundaries/sample_boundaries_v5.shp") %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  st_difference(exclusion)
street_network <- st_read("./data/raw/Street_Network_Database_SND/Street_Network_Database_SND.shp") %>%
  st_make_valid() %>%
  st_transform(4326)


freeway_buffer <- street_network %>% 
  st_transform(3689) %>%
  janitor::clean_names() %>%
  filter(snd_feacod == "Interstate Highway") %>%
  filter(structure != "Tunnel" & objectid %!in% c(33859, 32621, 32651)) %>%
  summarize(geometry = st_union(geometry)) %>%
  mutate(geometry = st_buffer(geometry, dist = units::set_units(100, "ft"))) %>%
  mutate(geometry = st_intersection(geometry, seattle_withwater %>% 
                                      st_transform(3689))) %>%
  st_transform(4326) %>%
  st_difference(exclusion)

freeway_periphery <- sample_boundaries %>%
  filter(str_detect(neighbplan, "Greenbelt|Add-On") & !str_detect(neighbplan, "Northlake Place|Kinnear Interlake Add-On")) %>%
  st_union() %>%
  st_union(freeway_buffer)

save(sample_boundaries, file = "./data/derived/sample_boundaries.RData")
save(freeway_buffer, file = "./data/derived/freeway_buffer.RData")
save(freeway_periphery, file = "./data/derived/freeway_periphery.RData")

resampled_area <- sample_boundaries %>% 
  st_make_valid() %>%
  st_geometry() %>% 
  st_union() %>% 
  st_union(freeway_buffer %>%  st_make_valid() %>% st_geometry()) %>% st_as_sf() %>% rename(geometry = x)

save(resampled_area, file = "./data/derived/resampled_area.RData")
