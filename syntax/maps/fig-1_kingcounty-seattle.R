# This script generates the simple maps of King County within Washington and
# Seattle within King County to provide readers with some context about the 
# study site. Seattle is a geographically interesting place! We're an isthmus!

library(tidyverse)
library(sf)
library(patchwork)
library(ggrepel)

# Could potentially make pretty using ggmagnify

load("./data/derived/seattle_nowater.RData")
load("./data/derived/sample_boundaries.RData")
load("./data/derived/king_county_nowater.RData")

source("./syntax/project_functions.R")

seattle_bbox <- seattle_nowater |> 
  st_bbox() 

panel_a <- ggplot() +
  geom_sf(data = king_county_nowater, color = "black", fill = "grey90", linewidth = 0.1) +
  geom_sf(data = seattle_nowater, color = NA, fill = "grey30") + 
  theme_void() +
  labs(title = "A. King County")

panel_b <- ggplot() +
  geom_sf(data = seattle_nowater, color = "black", fill = "grey90", linewidth = 0.1) +
  geom_sf(data = sample_boundaries, color = NA, fill = "grey30") + 
  theme_void() +
  labs(title = "B. Seattle")

combined_plot <- panel_a + panel_b + plot_layout(widths = c(1.5,1))
combined_plot
ggsave(filename = "fig/area_map.png", plot = combined_plot, device = ragg::agg_png, width = 5, height = 3.75, units = "in", dpi = 300)
