# This script generates the four-panel map of the locations of tents in the 
# census overlaid on built environment features, services and amenities, and
# ACS-measured poverty and non-white population. I tried to stick to colorblind-
# friendly palettes combined with varying shapes and line thicknesses, but it is
# difficult to guarantee it will be fully accessible. Virtually zero chance it
# will work in grayscale of course.

library(tidyverse)
library(sf)
library(patchwork)

load("./data/derived/open_spaces.RData")
load("./data/derived/zoning.RData")
load("./data/derived/line_features.RData")
load("./data/derived/tent_features.RData")
load("./data/derived/seattle_nowater.RData")
load("./data/derived/kc_water.RData")
load("./data/derived/resampled_area.RData")
load("./data/derived/seattle_big_buffer.RData")
load("./data/derived/acs5_bg_2020.RData")
load("./data/derived/point_features_for_map.RData")
source("./syntax/project_functions.R")

# Clean basic maps of where tents are with sample boundaries and roads.
# Ideal would be to fade out the unsampled area, which really means fading in
# the sampled area.

# Get the shapes for the areas sampled in each wave
sampled_area <- bind_rows(
  seattle_nowater |> 
    st_as_sf() |> 
    rename(geometry = x) |> 
    mutate(wave = 1), 
  resampled_area |> mutate(wave = 2), 
  resampled_area |> mutate(wave = 3))

# Areas not sampled in waves 2 and 3
unsampled_area <- sampled_area |>
  slice(2:3)

# Slice the sampled area out of the large seattle buffer
unsampled_area$geometry <- 
  rep(st_difference(seattle_big_buffer, sampled_area$geometry[2]), 2)

# Get relevant zoning with nice names for putting on the map; drop residential.
zoning_for_map <- zoning |>
  st_difference(kc_water) |>
  transmute(geometry, 
            feature = case_when(
              industrial           ~ "Industry",
              downtown             ~ "Downtown",
              commercial_mixed_use ~ "Commercial",
              multi_family         ~ "Residential",
              single_family        ~ "Residential",
              TRUE ~ NA)) |>
  filter(!is.na(feature) & feature != "Residential")

# Parks, greenbelts, trails
open_spaces_for_map <- open_spaces |>
  st_difference(kc_water) |> 
  filter(type %in% c("greenbelt", "park_garden_cemetery", "trail_or_roadside")) |>
           transmute(geometry, feature = "Green Space")

# Combine as they'll be plotted together
polygon_features_for_map <- bind_rows(open_spaces_for_map, zoning_for_map)

# Deal with that one problematic 1st ave bridge
first_bridge <- line_features |> 
  filter(type == "Bridge" & name %in% c("1ST AVE S / DUWAMISH RIVER", "1ST AVE S APPROACH")) |>
  st_cast(to = "LINESTRING", warn = FALSE)

# Handle all the roads and trails
line_features_for_map <- line_features %>%
  filter(type != "Pedestrian Bridge" & 
           type !="Bridge" & 
           !str_detect(address, "1ST AV S BR SB BETWEEN EAST MARGINAL WAY S") &
           !str_detect(name, "SR99 TUNNEL") &
           !(str_detect(name, "EAST MARGINAL") & 
               str_detect(address, "CORSON|CARLETON|FLORA|ELLIS|RIVER|14TH|16TH|BOEING DR|BRIGHTON|MICHIGAN"))) |>
  bind_rows(first_bridge) |>
  mutate(type = ifelse(str_detect(name, "AURORA|SR99|EAST MARGINAL|^SR509"), "Highway/Freeway", type),
         type = ifelse(type == "Bridge" & name %in% c("1ST AVE S / DUWAMISH RIVER", "1ST AVE S APPROACH"), "Highway/Freeway", type)) |>
  mutate(type = factor(type, levels = c("Highway/Freeway", "Arterial", "Collector", "Trail"))) %>%
  st_filter(seattle_big_buffer)

# Scale factor for manipulating the size of lines
scale_factor <- 1

# BUILT ENVIRONMENT PANEL
be_map <- ggplot() + 
  geom_sf(data = seattle_nowater, fill = "grey85", color = NA) + 
  geom_sf(data = zoning_for_map, aes(fill = feature), color = NA) +
  geom_sf(data = open_spaces_for_map, aes(fill = feature), color = NA) +
  scale_fill_manual("Land Use", values = c("Industry"="#DDCC77",
                               "Green Space" = "#117733",
                               "Downtown" = "#332288",
                               "Commercial" = "#88CCEE")) +
  geom_sf(data = line_features_for_map |> filter(type == "Highway/Freeway"), 
          aes(color = type), linewidth = 0.50*scale_factor) +
  geom_sf(data = tent_features |> filter(wave == 1), 
          aes(size = n_dwellings), color = "red", alpha = 0.75, shape = 16) +
  coord_sf(expand = FALSE) +
  scale_color_manual(NULL, values = c("Highway/Freeway" = "black")) +
  scale_size_continuous("Tents    ", breaks = c(1, 5, 20), range = c(0.5, 3)) +
  theme_void(base_size = 12) +
  labs(title="A. Built environment features") +
  theme(text = element_text(family = "serif"),
        legend.position = "right",
        panel.spacing = unit(0, "inches"),
        legend.title = element_text(size = 12),
        plot.title = element_text(margin = margin(0.1,0,0.05,0, unit = "inches"), 
                                  hjust = 0, size = 12))

be_map

# SERVICES PANEL
# Khroma palette for the service points
pal_khroma <- khroma::color("vibrant")(7)

services_map <- ggplot() + 
  geom_sf(data = seattle_nowater, fill = "grey85", color = NA) + 
  geom_sf(data = tent_features |> filter(wave == 1), 
          aes(size = n_dwellings), color = "red", alpha = 0.75, shape = 16) +
  scale_size_continuous("Tents    ", breaks = c(1, 5, 20), range = c(0.5, 3)) +
  geom_sf(data = point_features_for_map |>
            select(category, geometry) |>
            st_filter(seattle_nowater) |>
            filter(!category %in% c("School", "Police or Fire")) |>
            mutate(category = factor(category, 
                                     levels = c("Food, Housing, Hygiene, Shelters", 
                                                "Drug, Alcohol, Mental Health", 
                                                "Library", 
                                                "Grocery Store", 
                                                "Drug Store"))), 
          aes(color = category, shape = category), alpha = 1) +
  scale_shape_manual(NULL, values = c("Drug Store" = 15,
                                      "Drug, Alcohol, Mental Health" = 25, 
                                      "Food, Housing, Hygiene, Shelters" = 17, 
                                      "Library"  = 18,
                                      "Grocery Store" = 19),
                     labels = ~ str_wrap(., 18)) +
  scale_color_manual(NULL, values = c("Drug Store" = pal_khroma[1],
                                      "Drug, Alcohol, Mental Health" = pal_khroma[2], 
                                      "Food, Housing, Hygiene, Shelters" = pal_khroma[6], 
                                      "Library"  = "black",
                                      "Grocery Store" = "grey30"),
                     labels = ~ str_wrap(., 18)
  ) +
  coord_sf(expand = FALSE) +
  guides(shape = guide_legend(theme = theme(legend.key.spacing.y = unit(0.4, "line")))) +
  theme_void(base_size = 12) +
  labs(title="B. Services and amenities") +
  theme(text = element_text(family = "serif"),
        legend.position = "right",
        panel.spacing = unit(0, "inches"),
        legend.title = element_text(size = 12),
        plot.title = element_text(margin = margin(0.1,0,0.05,0, unit = "inches"), hjust = 0, size = 12))

services_map

# POVERTY PANEL
poverty_map <- ggplot() + 
  geom_sf(data = seattle_nowater, fill = "grey85", color = NA) + 
  geom_sf(data = acs5_bg_2020 %>%
            st_transform(st_crs(seattle_nowater)) %>%
            st_intersection(seattle_nowater), aes(fill = pr_poverty, color = pr_poverty)) +
  geom_sf(data = seattle_nowater, fill = NA, color = "grey50") +
  geom_sf(data = tent_features |> filter(wave == 1), aes(size = n_dwellings), color = "red", alpha = 0.75, shape = 16, show.legend=TRUE) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_c("Poverty", breaks = c(0, .20, .40, .60), labels = c("0%", "20%", "40%", "60%"), direction = -1) +
  scale_color_viridis_c("Poverty", breaks = c(0, .20, .40, .60), labels = c("0%", "20%", "40%", "60%"), direction = -1) +
  scale_size_continuous("Tents    ", breaks = c(1, 5, 20), range = c(0.5, 3)) +
  theme_void(base_size = 12) +
  labs(title="C. Percent of households under poverty line") +
  theme(text = element_text(family = "serif"),
        legend.position = "right",
        panel.spacing = unit(0, "inches"),
        legend.title = element_text(size = 12),
        plot.title = element_text(margin = margin(0.1,0,0.05,0, unit = "inches"), hjust = 0, size = 12))

poverty_map

# NONWHITE POP PANEL
nonwhite_map <- ggplot() + 
  geom_sf(data = seattle_nowater, fill = "grey85", color = NA) + 
  geom_sf(data = acs5_bg_2020 %>%
            mutate(pr_nonwhite = 1-pr_white) |>
            st_transform(st_crs(seattle_nowater)) %>%
            st_intersection(seattle_nowater), aes(fill = pr_nonwhite, color = pr_nonwhite)) +
  geom_sf(data = seattle_nowater, fill = NA, color = "grey50") +
  geom_sf(data = tent_features |> filter(wave == 1), aes(size = n_dwellings), color = "red", alpha = 0.75, shape = 16, show.legend=TRUE) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_c("Non-White", breaks = c(.20, .40, .60, .80), labels = c("20%", "40%", "60%", "80%"), direction = -1) +
  scale_color_viridis_c("Non-White", breaks = c(.20, .40, .60, .80), labels = c("20%", "40%", "60%", "80%"), direction = -1) +
  scale_size_continuous("Tents    ", breaks = c(1, 5, 20), range = c(0.5, 3)) +
  theme_void(base_size = 12) +
  labs(title="D. Percent non-white population") +
  theme(text = element_text(family = "serif"),
        legend.position = "right",
        panel.spacing = unit(0, "inches"),
        legend.title = element_text(size = 12),
        plot.title = element_text(margin = margin(0.1,0,0.05,0, unit = "inches"), hjust = 0, size = 12))

nonwhite_map


# Patchwork them together into a 2x2 of maps

city_map <- be_map +
  services_map + 
  poverty_map +
  nonwhite_map +
  plot_layout(ncol = 2, nrow =2) + 
  plot_annotation(theme = theme(plot.margin = margin(0,0,0,0, unit = "inches"))) 

city_map
ggsave(plot = city_map, filename = "./fig/city_map.png", dpi = 300, width = 12, height = 14)
