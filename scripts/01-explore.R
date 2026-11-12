## Setup

### Libraries
library(readxl)

#### tidyverse libraries
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

#### For loops
library(foreach)

#### Spatial data
library(terra)
library(sf)
library(fasterize)

#### Cross-referencing
library(ecoSynthesis)
library(geco)

### Paths
here::i_am("scripts/01-explore.R")

gisdata_dir <- here::here("data/source-data")
xref_dir <- here::here("data/xrefdata")
out_dir <- here::here("results", "output1-best-candidate-raster")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}
## Custom functions

source(here::here("scripts", "functions.R"))

## 1. Read Lookup table
lut_file <- here::here(
  gisdata_dir,
  "preeuropeanvegetationlutsummary-may2012.xlsx"
)

LUT <- read_xlsx(lut_file) |>
  mutate(
    `System-Association Code` = as.character(round(
      `System-Association Code`,
      1
    ))
  )

DPIRD_LUT <- lookup_tbl(
  LUT,
  id = "System-Association Code",
  name = "Original Association Description"
)
IUCN_GET <- lookup_tbl(IUCN_GET_LUT, "efg_id", "efg_name")

## 2. Read Membership tables

memb_data <- foreach(
  j = dir(xref_dir, pattern = ".csv"),
  .combine = bind_rows
) %do%
  {
    metadata <- read_csv(
      here::here(xref_dir, j),
      col_names = c("name", "value"),
      n_max = 12,
      show_col_types = F
    )[, 1:2]
    data <- read_csv(here::here(xref_dir, j), skip = 13, show_col_types = F)
    stringr::str_extract(j, "V[0-9]")
    cat(sprintf(
      "Read %s, interpreter %s\n%s\n",
      j,
      metadata[1, 2],
      metadata[12, 2]
    ))

    if ("EFG_1" %in% colnames(data)) {
      guess_round <- str_extract(j, "(?<=V)[0-9]")
      cat(sprintf("Assigning this to round %s\n", guess_round))

      pivot_wide_membership_table(
        xref = data,
        original_names = "System-Association Code",
        target_names = sprintf("EFG_%s", 1:4),
        membership_values = sprintf("memb_%s", 1:4),
        expert_name = metadata[[1, 2]],
        round = as.numeric(guess_round)
      )
    }
  }

memb_data |>
  filter(!is.na(memb), memb > 0) |>
  group_by(expert, round) |>
  summarise(
    `source ids` = n_distinct(`System-Association Code`),
    `target ids` = n_distinct(EFG),
    .groups = "drop"
  )


DPIRD_memb <- memb_data |>
  filter(!is.na(memb), memb > 0) |>
  mutate(
    `System-Association Code` = as.character(round(
      `System-Association Code`,
      1
    ))
  ) |>
  membership_tbl(
    source_id = "System-Association Code",
    target_id = "EFG",
    membership = "memb",
    interpreter = "expert",
    round = "round"
  )

check_membership(DPIRD_memb, source = DPIRD_LUT, target = IUCN_GET)

DPIRD_memb |>
  filter(!EFG %in% IUCN_GET$efg_id) |>
  summarise(
    n_distinct(`System-Association Code`),
    .by = c(expert, round, EFG)
  )

library(ggplot2)

DPIRD_memb |>
  filter(!EFG %in% IUCN_GET$efg_id) |>
  ggplot() +
  geom_histogram(aes(x = memb), bins = 5) +
  facet_grid(expert ~ round)


DPIRD_clean <- clean_membership(
  DPIRD_memb,
  source = DPIRD_LUT,
  target = IUCN_GET,
  action = "check"
)

DPIRD_best_summary <- summarise_membership(
  DPIRD_clean,
  keep_all = FALSE,
  which_round = 3
)

## 1. Read GIS data

DPIRD_xy <- read_sf(here::here(
  gisdata_dir,
  "Pre_European_Vegetation_DPIRD_006_WA_GDA2020_Public.gpkg"
)) |>
  mutate(sa_code = as.character(round(sa_code, 1))) |>
  st_transform(crs("EPSG:9473")) # Australian Albers

st_drop_geometry(DPIRD_xy) |> slice(10) |> glimpse()

st_drop_geometry(DPIRD_xy) |> slice(10) |> pull(struct_des)
st_drop_geometry(DPIRD_xy) |> slice(10) |> pull(flor_desc)

st_drop_geometry(DPIRD_xy) |> pull(sa_code) |> unique()


## Step 4: outputs

### Standard pixel values for Global Ecosystem Atlas
efg_info <- geco_efg_data() |>
  select(efg_code, efg_name, efg_pixel_value, efg_hex_code, efg_rgb_code) |>
  rename("EFG" = "efg_code")

efg_pixel_values <- efg_info |>
  select(EFG, efg_pixel_value)

### Export tabular lookup and synthesis tables
xref_best_tbl <- DPIRD_LUT |>
  as_tibble() |>
  left_join(DPIRD_best_summary, by = "System-Association Code") |>
  left_join(efg_info, by = "EFG") |>
  select(
    `System-Association Code`,
    `Original Association Description`,
    EFG,
    efg_name,
    efg_pixel_value,
    efg_hex_code,
    efg_rgb_code,
    round,
    prop_memb,
    total_memb,
    supporting,
    total_interp
  )

write_csv(
  xref_best_tbl,
  here::here(out_dir, "DPIRD-xref-best-candidate.csv")
)

efg_lookup_tbl <- xref_best_tbl |>
  filter(!is.na(EFG)) |>
  summarise(
    n_DPIRD_units = n_distinct(`System-Association Code`),
    .by = c(efg_pixel_value, EFG, efg_name, efg_hex_code, efg_rgb_code)
  ) |>
  left_join(
    as_tibble(IUCN_GET) |>
      select(efg_id, realm_id, realm_name, biome_id, biome_name),
    by = c("EFG" = "efg_id")
  ) |>
  select(
    efg_pixel_value,
    EFG,
    efg_name,
    realm_id,
    realm_name,
    biome_id,
    biome_name,
    efg_hex_code,
    efg_rgb_code,
    n_DPIRD_units
  ) |>
  arrange(efg_pixel_value)

write_csv(
  efg_lookup_tbl,
  here::here(out_dir, "IUCN-GET-pixel-lookup.csv")
)

clr_lines <- efg_lookup_tbl |>
  tidyr::separate(efg_rgb_code, into = c("R", "G", "B"), sep = ",") |>
  transmute(
    line = sprintf(
      "%d %s %s %s 255 %s %s",
      efg_pixel_value,
      R,
      G,
      B,
      EFG,
      efg_name
    )
  ) |>
  pull(line)

writeLines(
  clr_lines,
  here::here(out_dir, "DPIRD-xref-target.clr")
)

### Combined attribute table (vector data)
DPIRD_xy_best <- DPIRD_xy |>
  left_join(
    DPIRD_best_summary,
    by = c("sa_code" = "System-Association Code"),
    relationship = "many-to-one"
  ) |>
  left_join(efg_pixel_values, by = "EFG")

### template raster file
rez <- 90
DPIRD_raster <- raster(DPIRD_xy, res = rez)

### Vector to raster using `fasterize`

#### Best candidate

target_raster <- fasterize(
  DPIRD_xy_best,
  DPIRD_raster,
  field = "efg_pixel_value",
  fun = "first"
) |>
  rast()

ctab <- geco_efg_data() |>
  transmute(value = efg_pixel_value, color = efg_hex_code)
coltab(target_raster) <- ctab
out_file <- here::here(out_dir, "DPIRD-xref-target.tiff")

rast_metadata <- c(
  PROJECT = "Ecosystem Indicators Workflows",
  TITLE = "Best candidate ecosystem function group",
  DESCRIPTION = "Synthesis of crossreference from DPIRD 006 to IUCN GET"
)
metags(target_raster) <- rast_metadata

writeRaster(target_raster, out_file, overwrite = TRUE)
