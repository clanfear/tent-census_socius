# Get 2020 ACS data at tract and blockgroup level
library(tidyverse)
library(sf)
library(tidycensus)
source("./syntax/project_functions.R")

load("./data/derived/seattle_tract_boundaries.RData")
load("./data/derived/seattle_bg_boundaries.RData")

acs5_2020_vars <- load_variables(2020, "acs5", cache=TRUE)

var_vector <- c("C17002_001", "C17002_002", "C17002_003", # % under poverty line
                "B19057_001", "B19057_002", # %public assistance
                "B11001_001", "B11001_006", # % FHH
                "B23025_001", "B23025_005", # % unemployed
                "B01001_001" , "B01001_003", "B01001_004", "B01001_005", "B01001_006", # % under 18 M
                               "B01001_027", "B01001_028", "B01001_029", "B01001_030", # % under 18 F
                "B02001_001", "B02001_003", # % black
                              "B02001_002", #  count white
                              "B02001_005", #  count asian
                              "B02001_006", "B02001_004", #  count NHOPI, AIAN
                "B05002_001", "B05002_013", # foreign born
                "B03001_001", "B03001_003", # hispanic or latino
                "B25003_001", "B25003_002", # owner occupied
                "B23010_001", "B23010_002", # own children under 18
                "B01003_001", # population
                "B19126_001", # median family income
                "B07013_001", "B07013_004" # Same house 1 year ago
                ) 

# TRACT
acs5_tract_2020 <- get_acs("tract", variables = var_vector, year = 2020, state = "WA",
        county = "King", output = "wide", geometry = TRUE) |>
  rename(tract = GEOID) |>
  filter(tract %in% seattle_tract_boundaries$tract) |>
  select(-ends_with("M")) |>
  rename_with(~ str_remove(., "E$"), .cols = c(-tract, -NAME)) |>
  transmute(tract,
            pop           = B01003_001,
            median_inc    = B19126_001,
            pr_poverty    = (C17002_002 + C17002_003) / C17002_001,
            pr_pub_assist = B19057_002 / B19057_001,
            pr_fhh        = B11001_006 / B11001_001,
            pr_under_18   = (B01001_003 + B01001_004 + B01001_005 + B01001_006 + B01001_027 + B01001_028 + B01001_029 + B01001_030) / B01001_001,
            pr_white      = B02001_002 / B02001_001,
            pr_black      = B02001_003 / B02001_001,
            pr_asian      = B02001_005 / B02001_001,
            pr_nhopi      = B02001_006 / B02001_001,
            pr_aian       = B02001_004 / B02001_001,
            pr_foreign    = B05002_013 / B05002_001,
            pr_hisp       = B03001_003 / B03001_001,
            pr_ownhome    = B25003_002 / B25003_001,
            pr_children   = B23010_002 / B23010_001,
            pr_unemp      = B23025_005 / B23025_001,
            pr_same_house = B07013_004 / B07013_001,
            geometry = geometry
            ) %>% 
  st_as_sf() %>%
  mutate(., area_sqkm = as.numeric(units::set_units(st_area(.), "mi^2"))) |>
  mutate(pop_sqkm = as.numeric(pop / area_sqkm)) %>% 
  mutate(.,
         disadvantage = psych::principal(select(st_drop_geometry(.), pr_pub_assist, pr_poverty, pr_unemp, median_inc))$scores[,1]
  )

save(acs5_tract_2020, file = "./data/derived/acs5_tract_2020.RData")

# BG
# Note: Blockgroups 53.03.2 and 53.04.1 are campus and are missing non-race info
# 530330086003 excluded because it has 19 people valid for disadvantage measures
acs5_bg_2020 <- get_acs("block group", variables = var_vector, year = 2020, state = "WA",
                          county = "King", output = "wide", geometry = TRUE) |>
  rename(blockgroup = GEOID) |>
  filter((as.numeric(blockgroup) < 530330122000 & as.numeric(blockgroup) > 530330001000) | blockgroup == 530330265001) |>
  select(-ends_with("M")) |>
  rename_with(~ str_remove(., "E$"), .cols = c(-blockgroup, -NAME)) |>
  transmute(blockgroup,
            pop           = B01003_001,
            pop_pov       = C17002_001,
            pr_poverty    = (C17002_002 + C17002_003) / C17002_001,
            pr_pub_assist = B19057_002 / B19057_001,
            pr_fhh        = B11001_006 / B11001_001,
            pr_under_18   = (B01001_003 + B01001_004 + B01001_005 + B01001_006 + B01001_027 + B01001_028 + B01001_029 + B01001_030) / B01001_001,
            pr_white      = B02001_002 / B02001_001,
            # pr_hisp       = B03001_003 / B03001_001, # Not available at BG level
            pr_black   = B02001_003 / B02001_001,
            pr_asian   = B02001_005 / B02001_001,
            pr_nhopi   = B02001_006 / B02001_001,
            pr_aian    = B02001_004 / B02001_001,
            pr_unemp      = B23025_005 / B23025_001,
            pr_ownhome = B25003_002 / B25003_001,
            geometry = geometry
           ) %>% 
  st_as_sf() %>%
  mutate(., area_sqkm = as.numeric(units::set_units(st_area(.), "mi^2"))) |>
  mutate(pop_sqkm = as.numeric(pop / area_sqkm)) |>
  filter(!is.na(pr_poverty) & !is.nan(pr_poverty) & blockgroup != 530330086003) %>% 
  mutate(.,
         disadvantage = psych::principal(select(st_drop_geometry(.), pr_pub_assist, pr_poverty, pr_unemp))$scores[,1]
  )
save(acs5_bg_2020, file = "./data/derived/acs5_bg_2020.RData")


