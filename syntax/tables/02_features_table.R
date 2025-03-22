library(tidyverse)
library(sf)
library(flextable)

load("./data/derived/tent_features.RData")

tent_features_nogeo <- tent_features %>% 
  st_drop_geometry() %>%
  mutate(size = factor(case_when(
    n_dwellings ==1 ~ "Single Tent",
    n_dwellings > 1 & n_dwellings <= 3  ~ "Small Camp (2-3)",
    n_dwellings > 3 & n_dwellings <= 6 ~ "Medium Camp (4-6)",
    n_dwellings > 6 ~ "Large Camp (7+)"
  ), levels = c("Single Tent", "Small Camp (2-3)", "Medium Camp (4-6)", "Large Camp (7+)" )))


# Table 2: Tent camp size by physical and social features
feature_table <- tent_features_nogeo |>
  filter(wave == 1) |>
  transmute(size,
         `Social Services` = hygiene_services | housing_assistance | shelters |food_assistance | drug_and_alcohol_services | mental_health_or_substance_use,
         Industrial = industrial,
         Downtown = downtown,
         `Green Spaces` = trail_or_roadside | park_garden_cemetery | greenbelt | trail,
         `Freeway` = freeway_periphery | highway_freeway,
         Total = TRUE) |>
  pivot_longer(-size) |>
  summarize(`N (%)` = str_c(sum(value), " (", 100*round(sum(value)/n(),2), "%)"),.by = c(size, name)) |>
  pivot_wider(names_from = size, values_from = `N (%)`) |>
  rename(Feature=name) |>
  mutate(Feature = factor(Feature, levels = c("Industrial", "Downtown", "Freeway", "Green Spaces", "Social Services", "Total"))) |>
  arrange(Feature) |>
  flextable() |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 11, part = "all") |>
  width(j = c(1, 3,5), width = 1.3, unit = "in") |>
  width(j = 2, width = 1.15, unit = "in") |>
  width(j = 4, width = 1.45, unit = "in") |>
  border_remove() |>
  hline(i = 5, border = officer::fp_border(width = 0.5)) |>
  hline(i = 6, border = officer::fp_border(width = 1)) |>
  hline_top(border = officer::fp_border(width = 1), part = "body") |>
  add_header_lines(values = c("Table 2: Tent camp size by physical and social features")) 

feature_table
print(feature_table, preview = 'docx')

# Table 3: Tents within resample area by camp size and period
resample_table <- 
  tent_features_nogeo |>
  filter(resample_area) |>
  select(wave, size, n_dwellings) |>
  summarize(N = sum(n_dwellings), .by = c(wave, size)) |>
  mutate(`N (%)`= str_c(N, " (", 100*round(N / sum(N),2), "%)"), .by = wave) |>
  mutate(Total = str_c(sum(N), " (100%)"), .by = wave) |>
  select(-N) |>
  pivot_wider(names_from = size, values_from = `N (%)`) |>
  pivot_longer(-wave) |>
  mutate(Period = case_when(
    wave == 1 ~  "Summer 2019",
    wave == 2 ~  "Autumn 2019",
    wave == 3 ~  "Summer 2020"
  )) |>
  select(-wave) |>
  pivot_wider(names_from = Period, values_from = value) |>
  select(`Camp size` = name, `Summer 2019`, `Autumn 2019`, `Summer 2020`) |>
  mutate(`Camp size` = factor(`Camp size`, levels = c("Single Tent", "Small Camp (2-3)", "Medium Camp (4-6)", "Large Camp (7+)", "Total"))) |>
  arrange(`Camp size`) |>
  flextable() |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 11, part = "all") |>
  autofit() |>
  border_remove() |>
  hline(i = 4, border = officer::fp_border(width = 0.5)) |>
  hline_top(border = officer::fp_border(width = 1), part = "body") |>
  hline_bottom(border = officer::fp_border(width = 1), part = "body") |>
  add_header_lines(values = c("Table 3: Tents within resample area by camp size and period ")) 

resample_table
print(resample_table, preview = 'docx')

# Table 4: Total tent camps by camp size and period
resample_table_2 <- 
  tent_features_nogeo |>
  filter(resample_area) |>
  select(wave, size, n_dwellings) |>
  summarize(N = n(), .by = c(wave, size)) |>
  filter(size != "Single Tent") |>
  mutate(`N (%)`= str_c(N, " (", 100*round(N / sum(N),2), "%)"), .by = wave) |>
  mutate(Total = str_c(sum(N), " (100%)"), .by = wave) |>
  select(-N) |>
  pivot_wider(names_from = size, values_from = `N (%)`) |>
  pivot_longer(-wave) |>
  mutate(Period = case_when(
    wave == 1 ~  "Summer 2019",
    wave == 2 ~  "Autumn 2019",
    wave == 3 ~  "Summer 2020"
  )) |>
  select(-wave) |>
  pivot_wider(names_from = Period, values_from = value) |>
  select(`Camp size` = name, `Summer 2019`, `Autumn 2019`, `Summer 2020`) |>
  mutate(`Camp size` = factor(`Camp size`, levels = c("Small Camp (2-3)", "Medium Camp (4-6)", "Large Camp (7+)", "Total"))) |>
  arrange(`Camp size`) |>
  flextable() |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 11, part = "all") |>
  autofit() |>
  border_remove() |>
  hline(i = 3, border = officer::fp_border(width = 0.5)) |>
  hline_top(border = officer::fp_border(width = 1), part = "body") |>
  hline_bottom(border = officer::fp_border(width = 1), part = "body") |>
  add_header_lines(values = c("Table 4: Total tent camps by camp size and period")) 

resample_table_2
print(resample_table_2, preview = 'docx')



