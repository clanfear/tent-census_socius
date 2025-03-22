# Generates table for waves of data collection
library(tidyverse)
library(sf)
library(flextable)

load("./data/derived/tent_features.RData")

tent_features_nogeo <- tent_features |> 
  st_drop_geometry() |>
  mutate(size = factor(case_when(
    n_dwellings ==1 ~ "Single",
    n_dwellings <= 4 ~ "Small Camp",
    n_dwellings >= 5 ~ "Large Camp"
  ), levels = c("Single", "Small Camp", "Large Camp" )))


# Table of tent size – 1, 2, 3, 4, 5, 6, 7+ with numbers
encampment_size_table <- tent_features_nogeo |>
  filter(freeway_or_resample) |>
  mutate(n_dwellings = ifelse(n_dwellings > 6, "7+", n_dwellings)) |>
  count(wave, n_dwellings) |>
  complete(wave, n_dwellings, fill = list(n=0)) |>
  group_by(wave) |>
  mutate(prop = n/sum(n)) |>
  ungroup() |>
  transmute(wave = wave, `# Tents` = n_dwellings, n_prop = paste0(n, "\n(", round(prop,2), ")")) |>
  pivot_wider( names_from = wave, values_from = n_prop) |>
  flextable() |>
  add_header_lines("Tents per encampment by wave") |>
  add_footer_lines("Only in resample area")

encampment_size_table_data <- tent_features_nogeo |>
  filter(wave==1) |>
  mutate(n_dwellings_chr = ifelse(n_dwellings > 6, "7+", n_dwellings)) |>
  summarize(n = n(), 
            tent_count = sum(n_dwellings),
            .by = n_dwellings_chr) |>
  mutate(n = ifelse(n_dwellings_chr == "7+", n+1, n),
         tent_count = ifelse(n_dwellings_chr == "7+", tent_count+40, tent_count)) |>
  mutate(pr_n = 100*n/sum(n),
         pr_tents = 100*tent_count / sum(tent_count)) |>
  arrange(n_dwellings_chr)

table_foot <- encampment_size_table_data |>
  summarize(across(-n_dwellings_chr, ~sum(.))) |>
  transmute(`N Encampments (%)` = paste0(n, "\n(", round(pr_n), "%)"),
            `Total tents (%)` = paste0(tent_count, "\n(", round(pr_tents), "%)")) |>
  as.list() %>%
  c(list(`# Tents`= "Total"), .)

encampment_size_table <- encampment_size_table_data |>
  transmute(`# Tents` = n_dwellings_chr, 
            `N Encampments (%)` = paste0(n, "\n(", round(pr_n), "%)"),
            `Total tents (%)` = paste0(tent_count, "\n(", round(pr_tents), "%)")) |>
  arrange(`# Tents`) |>
  flextable() |>
  add_footer_row(values = table_foot, colwidths = c(1,1,1)) |>
  add_header_lines("Tents per encampment in census")

# Render to a temporary Word doc for pasting into the draft, since we aren't
# using a Quarto doc.
print(encampment_size_table, preview = 'docx')
