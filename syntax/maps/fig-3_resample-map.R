# This script generates the map of tent locations in each of the three waves of
# data collection, with the resample areas highlighted to permit comparison over
# time.

library(tidyverse)
library(sf)
library(patchwork)

load("./data/derived/seattle_nowater.RData")
load("./data/derived/resampled_area.RData")
source("./syntax/project_functions.R")

sample_map <- ggplot() + 
  geom_sf(data = seattle_nowater, fill = "grey85", color = NA) + 
  geom_sf(data = bind_rows(resampled_area |> mutate(wave =1),
                           resampled_area |> mutate(wave =2),
                           resampled_area |> mutate(wave =3)), aes(fill = "Resample\nArea")) +
  scale_fill_manual(NULL, values = c("Resample\nArea" = "#648FFF")) +
  geom_sf(data = tent_features, aes(size = n_dwellings), color = "red", alpha = 0.75, shape = 16) +
  scale_size_continuous("Tents    ", breaks = c(1, 5, 20), range = c(0.5, 3)) +
  facet_wrap(~wave, labeller = labeller(wave = c("1" = "Census - Summer 2019", 
                                                 "2" = "Resample - Autumn 2019", 
                                                 "3" = "Resample - Summer 2020")))+
  coord_sf(expand = FALSE) +
  theme_void(base_size = 12) +
  theme(text = element_text(family = "serif"),
        legend.position = "right",
        panel.spacing = unit(0, "inches"),
        strip.text = element_text(margin = margin(0,0,0.1,0, unit = "inches"), hjust = 0, size = 12),
        plot.title = element_text(margin = margin(0.1,0,0.05,0, unit = "inches"), hjust = 0, size = 12))

sample_map

ggsave(plot = sample_map, filename = "./fig/sample_map.png", dpi = 300, width = 12, height = 6)
