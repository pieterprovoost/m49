library(dplyr)
library(xlsx)

m49_full <- read.xlsx("./inst/csv/UNSD — Methodology.xlsx", sheetIndex = 1, stringsAsFactors = FALSE)

# fix names

names(m49_full) <- c("global_code", "global_name", "region_code", "region_name", "subregion_code", "subregion_name", "intermediateregion_code", "intermediateregion_name", "country_or_area", "m49_code", "iso_alpha3_code", "ldc", "lldc", "sids", "developed_developing")

# fix boolean columns

m49_full <- m49_full %>%
  mutate(
    ldc = ifelse(!is.na(ldc) & ldc == "x", TRUE, FALSE),
    lldc = ifelse(!is.na(lldc) & lldc == "x", TRUE, FALSE),
    sids = ifelse(!is.na(sids) & sids == "x", TRUE, FALSE),
    developed = ifelse(!is.na(developed_developing) & developed_developing == "Developed", TRUE, FALSE),
    developing = ifelse(!is.na(developed_developing) & developed_developing == "Developing", TRUE, FALSE)
  ) %>%
  select(-developed_developing)

# extract regions and countries

areas_global <- m49_full %>%
  select(code = global_code, name = global_name) %>%
  distinct() %>%
  mutate(parent = NA)

areas_regions <- m49_full %>%
  select(code = region_code, name = region_name, parent = global_code) %>%
  distinct() %>%
  filter(!is.na(code))

areas_subregions <- m49_full %>%
  select(code = subregion_code, name = subregion_name, parent = region_code) %>%
  distinct() %>%
  filter(!is.na(code))

areas_intermediateregions <- m49_full %>%
  select(code = intermediateregion_code, name = intermediateregion_name, parent = subregion_code) %>%
  distinct() %>%
  filter(!is.na(code))

areas_country <- m49_full %>%
  mutate(
    parent = ifelse(
      !is.na(intermediateregion_code),
      intermediateregion_code,
      ifelse(
        !is.na(subregion_code),
        subregion_code,
        ifelse(
          !is.na(region_code),
          region_code,
          global_code
        )
      )
    )
  ) %>%
  select(
    code = m49_code,
    name = country_or_area,
    iso_alpha3_code,
    ldc,
    lldc,
    sids,
    developed,
    developing,
    parent
  ) %>%
  distinct() %>%
  filter(!is.na(code)) %>%
  arrange(code)

m49_regions <- bind_rows(areas_global, areas_regions, areas_subregions, areas_intermediateregions)
m49_countries <- areas_country

# output

save(m49_full, file = "./data/m49_full.RData")
save(m49_regions, file = "./data/m49_regions.RData")
save(m49_countries, file = "./data/m49_countries.RData")



