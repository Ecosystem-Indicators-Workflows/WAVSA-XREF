## The purpose of this script is to create a spatial output from source
## spatial data using cross-referencing information consolidated in
## membership tables.

## Source classification and spatial data (units):
## > DPIRD 006 (vegetation associations)
## Target classification (units):
## > IUCN Global Ecosystem Typology (ecosystem functional groups)
## Output format:
## > Raster GeoTiff files with best candidate target unit, membership and
## > number of supporting interpreters

## This script summarises the minimum set of steps to complete four stages
## of the cross-referencing workflow:
## 1) Data preparation
## 2) Data validation
## 3) Synthesis
## 4) Read additional data
## 5) Outputs in preferred format

## Recommended usage:
## R --vanilla < scripts/10-create-output-best-candidate.R

## Step O: Setup

### Benchmarking
start_time <- Sys.time()

### Libraries

#### tidyverse libraries
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

#### Read and write other formats
library(readxl)
library(foreign)
library(jsonlite)

#### For loops
library(foreach)

#### Plots
# library(ggplot2)

#### Spatial data
library(terra)
library(sf)
library(fasterize)

#### Cross-referencing
library(ecoSynthesis)
library(geco)

### Paths
here::i_am("scripts/10-create-output-best-candidate.R")

gisdata_dir <- here::here("data/source-data")
xref_dir <- here::here("data/xrefdata")
out_dir <- here::here("results", "output1-best-candidate-raster")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

### Custom functions

source(here::here("scripts", "functions.R"))

### Predefined parameters

the_round <- 3
epsg_code <- "EPSG:9473" # Australian Albers


## Step 1: data preparation

### Lookup table source classification

lut_file <- here::here(
  gisdata_dir,
  "preeuropeanvegetationlutsummary-may2012.xlsx"
)

LUT <- read_xlsx(lut_file) |>
  mutate(
    `System-Association Code` = as.character(round(
      `System-Association Code`,
      2
    )),
    eligible = TRUE
  )

DPIRD_LUT <- lookup_tbl(
  LUT,
  id = "System-Association Code",
  name = "Original Association Description",
  eligible = "eligible"
)

### Lookup table target classification

IUCN_GET <- lookup_tbl(IUCN_GET_LUT, "efg_id", "efg_name")

### Membership tables

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

## Step 2: data validation

DPIRD_memb <- memb_data |>
  filter(!is.na(memb), memb > 0) |>
  mutate(
    `System-Association Code` = as.character(round(
      `System-Association Code`,
      2
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

DPIRD_clean <- clean_membership(
  DPIRD_memb,
  source = DPIRD_LUT,
  target = IUCN_GET,
  action = "check"
)

DPIRD_clean |>
  mutate(dups = grepl("duplicated ass", flag)) |>
  summarise(
    source_units = n_distinct(`System-Association Code`),
    .by = c(expert, round, dups)
  )

DPIRD_clean <- DPIRD_clean |> filter(!grepl("duplicated record", flag))

## Step 3: synthesis

DPIRD_best_summary <- summarise_membership(
  DPIRD_clean,
  keep_all = FALSE,
  which_round = the_round
)

## Step 4: read additional data for outputs

### 4a. Read GIS data

DPIRD_xy <- read_sf(here::here(
  gisdata_dir,
  "Pre_European_Vegetation_DPIRD_006_WA_GDA2020_Public.gpkg"
)) |>
  mutate(sa_code = as.character(round(sa_code, 1))) |>
  st_transform(crs(epsg_code))


### 4b. Standard pixel values for Global Ecosystem Atlas
### + the T5.6

t5_6_info <- tibble(
  EFG = "T5.6",
  efg_name = "Semi-arid woodlands",
  efg_pixel_value = 35,
  efg_hex_code = "#EEB4B4",
  efg_rgb_code = "238,180,180"
) #rosybronw2

efg_info <- geco_efg_data() |>
  select(efg_code, efg_name, efg_pixel_value, efg_hex_code, efg_rgb_code) |>
  rename("EFG" = "efg_code") |>
  bind_rows(t5_6_info) |>
  arrange(efg_pixel_value)

efg_pixel_values <- efg_info |>
  select(EFG, efg_pixel_value)

## Step 5: export outputs

xref_best_file <- "DPIRD-xref-best-candidate.csv"
efg_lookup_file <- "IUCN-GET-pixel-lookup.csv"
xref_clr_file <- "DPIRD-xref-target.clr"
xref_aux_file <- "DPIRD-xref-target.tiff.aux.xml"
xref_qml_file <- "DPIRD-xref-target.qml"
xref_tiff_file <- "DPIRD-xref-target.tiff"
memb_tiff_file <- "DPIRD-xref-membership.tiff"
supp_tiff_file <- "DPIRD-xref-supporting.tiff"

### 5a. Export tabular synthesis tables

xref_best_tbl <- DPIRD_LUT |>
  as_tibble() |>
  left_join(DPIRD_best_summary, by = "System-Association Code") |>
  left_join(efg_info, by = "EFG") |>
  select(
    `System-Association Code`,
    `Original Association Description`,
    eligible,
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
  here::here(out_dir, xref_best_file)
)


### 5b. Export tabular lookup  tables

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
  here::here(out_dir, efg_lookup_file)
)

### 5c. Export colour file

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
  here::here(out_dir, xref_clr_file)
)

### 5d. Export style file

qml_entries <- efg_lookup_tbl |>
  transmute(
    entry = sprintf(
      '        <paletteEntry value="%d" color="%s" alpha="255" label="%s %s"/>',
      efg_pixel_value,
      efg_hex_code,
      EFG,
      efg_name
    )
  ) |>
  pull(entry)

qml_lines <- c(
  "<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>",
  '<qgis version="3.34.0" minScale="1e+08" maxScale="0" hasScaleBasedVisibilityFlag="0" styleCategories="AllStyleCategories">',
  "  <pipe>",
  "    <provider>",
  '      <resampling enabled="false" maxOversampling="2" zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour"/>',
  "    </provider>",
  '    <rasterrenderer band="1" type="paletted" opacity="1" alphaBand="-1" nodataColor="">',
  "      <rasterTransparency/>",
  "      <minMaxOrigin>",
  "        <limits>None</limits>",
  "        <extent>WholeRaster</extent>",
  "        <statAccuracy>Estimated</statAccuracy>",
  "        <cumulativeCutLower>0.02</cumulativeCutLower>",
  "        <cumulativeCutUpper>0.98</cumulativeCutUpper>",
  "        <stdDevFactor>2</stdDevFactor>",
  "      </minMaxOrigin>",
  "      <colorPalette>",
  qml_entries,
  "      </colorPalette>",
  "    </rasterrenderer>",
  '    <brightnesscontrast brightness="0" contrast="0" gamma="1"/>',
  '    <huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>',
  '    <rasterresampler maxOversampling="2"/>',
  "  </pipe>",
  "  <blendMode>0</blendMode>",
  "</qgis>"
)
writeLines(qml_lines, here::here(out_dir, xref_qml_file))


### 5f. Export raster files
### Convert vector to raster using `fasterize`

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

#### Best candidate

target_raster <- fasterize(
  DPIRD_xy_best,
  DPIRD_raster,
  field = "efg_pixel_value",
  fun = "first"
) |>
  rast()

ctab <- efg_info |>
  filter(EFG %in% DPIRD_xy_best$EFG) |>
  transmute(value = efg_pixel_value, color = efg_hex_code)
coltab(target_raster) <- ctab

rast_metadata <- c(
  PROJECT = "Ecosystem Indicators Workflows",
  TITLE = "Best candidate ecosystem functional group",
  DESCRIPTION = "Synthesis of crossreference from DPIRD 006 to IUCN GET"
)
metags(target_raster) <- rast_metadata
raster_attributes <- efg_lookup_tbl |>
  transmute(
    id = coalesce(efg_pixel_value, 255L),
    label = paste(EFG, efg_name, sep = " ")
  )
levels(target_raster) <- raster_attributes
writeRaster(
  target_raster,
  here::here(out_dir, xref_tiff_file),
  overwrite = TRUE
)

# out_file <- here::here(out_dir, "DPIRD-xref-target.tiff.vat.dbf")
# write.dbf(data.frame(raster_attributes), file = out_file)

#### Best candidate membership

memb_raster <- fasterize(
  DPIRD_xy_best,
  DPIRD_raster,
  field = "prop_memb",
  fun = "max"
) |>
  rast()

rast_metadata[
  "TITLE"
] <- "Membership for the best candidate ecosystem functional group"

metags(memb_raster) <- rast_metadata

writeRaster(memb_raster, here::here(out_dir, memb_tiff_file), overwrite = TRUE)

#### Supporting interpreters

supp_raster <- fasterize(
  DPIRD_xy_best,
  DPIRD_raster,
  field = "supporting",
  fun = "max"
) |>
  rast()

rast_metadata["TITLE"] <- "Nr. of interpreters supporting assignment"

metags(supp_raster) <- rast_metadata

writeRaster(supp_raster, here::here(out_dir, supp_tiff_file), overwrite = TRUE)


## Step X: Benchmarking and session info

end_time <- Sys.time()
end_time - start_time

sessionInfo()
