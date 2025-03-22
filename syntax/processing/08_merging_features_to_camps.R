# This script merges characteristics of locations on to individual camps.
# This is suitable for comparing different camps. This is not suitable for
# comparing different locations. Analyses here will all use the same unit, the
# encampment, so they're more comparable than using a dozen different types of
# area.

library(tidyverse)
library(sf)
source("./syntax/project_functions.R")

load("./data/derived/tent_census.RData")
load("./data/derived/freeway_buffer.RData")
load("./data/derived/sample_boundaries.RData")
load("./data/derived/freeway_periphery.RData")
load("./data/derived/resampled_area.RData")
load("./data/derived/seattle_block_boundaries.RData")
load("./data/derived/acs5_tract_2019.RData")

load("./data/derived/zoning.RData")
load("./data/derived/point_features_for_join.RData")
load("./data/derived/open_spaces_for_join.RData")
load("./data/derived/line_features_for_join.RData")

# For boundaries, should be about being within the boundary. Buffers are added 
# for parks and things where slight coding inaccuracy may place it outside that 
# border.
# Remember: If using a buffer, convert to a proper projection. s2 doesn't like 
# lat / lon buffers.

# Get admin boundaries first
tent_features <- tent_census |>
  st_join(acs5_tract_2019 |> st_transform(4326), join = st_nearest_feature) |>
  st_join(resampled_area |> mutate(resample_area = TRUE)) |>
  st_join(st_as_sf(freeway_buffer) |> transmute(geometry, freeway_area = TRUE)) |>
  mutate(resample_area = ifelse(is.na(resample_area), FALSE, resample_area),
         freeway_area = ifelse(is.na(freeway_area), FALSE, freeway_area),
         freeway_or_resample = resample_area | freeway_area) |>
  left_join(
    tent_census |>
      select(id, geometry) |>
      st_join(open_spaces_for_join) |>
      st_drop_geometry() |>
      select(-type) |>
      group_by(id) |>
      # If any feature is TRUE, set TRUE
      summarize(across(everything(), ~ifelse(any(.) %in% TRUE, TRUE, FALSE)))
  ) |>
  left_join(
    tent_census |>
      select(id, geometry) |>
      st_join(line_features_for_join) |>
      st_drop_geometry() |>
      select(-type) |>
      group_by(id) |>
      summarize(across(everything(), ~ifelse(any(.) %in% TRUE, TRUE, FALSE)))
  ) |>
  left_join(
    tent_census |>
      select(id, geometry) |>
      st_join(point_features_for_join) |>
      st_drop_geometry() |>
      select(-type, -category) |>
      group_by(id) |>
      summarize(across(everything(), ~ifelse(any(.) %in% TRUE, TRUE, FALSE)))
  ) |>
  st_join(zoning, join = st_nearest_feature) |>
  st_join(freeway_periphery |> 
            st_as_sf() |> 
            rename(geometry = x) |> 
            mutate(freeway_periphery = TRUE)) |>
  mutate(freeway_periphery = ifelse(is.na(freeway_periphery), FALSE, freeway_periphery))

save(tent_features, file = "./data/derived/tent_features.RData")




  