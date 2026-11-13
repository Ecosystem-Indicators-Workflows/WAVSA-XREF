## Setup

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
here::i_am("scripts/01-explore.R")

gisdata_dir <- here::here("data/source-data")
xref_dir <- here::here("data/xrefdata")
out_dir <- here::here("results", "output1-best-candidate-raster")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

### Custom functions

source(here::here("scripts", "functions.R"))

### metadata templates

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

# DPIRD_memb |>
#   filter(!EFG %in% IUCN_GET$efg_id) |>
#   ggplot() +
#   geom_histogram(aes(x = memb), bins = 5) +
#   facet_grid(expert ~ round)

DPIRD_clean <- clean_membership(
  DPIRD_memb,
  source = DPIRD_LUT,
  target = IUCN_GET,
  action = "check"
)

the_round <- 3
DPIRD_best_summary <- summarise_membership(
  DPIRD_clean,
  keep_all = FALSE,
  which_round = the_round
)

## 1. Read GIS data

epsg_code <- "EPSG:9473"

DPIRD_xy <- read_sf(here::here(
  gisdata_dir,
  "Pre_European_Vegetation_DPIRD_006_WA_GDA2020_Public.gpkg"
)) |>
  mutate(sa_code = as.character(round(sa_code, 1))) |>
  st_transform(crs(epsg_code)) # Australian Albers

# st_drop_geometry(DPIRD_xy) |> slice(10) |> glimpse()

# st_drop_geometry(DPIRD_xy) |> slice(10) |> pull(struct_des)
# st_drop_geometry(DPIRD_xy) |> slice(10) |> pull(flor_desc)

# st_drop_geometry(DPIRD_xy) |> pull(sa_code) |> unique()

## Step 4: outputs

### Standard pixel values for Global Ecosystem Atlas

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


dtstr <- sapply(xref_best_tbl, class)

rs <- list()
rs[[1]] <- list(
  name = "Crossreference summary",
  path = "DPIRD-xref-best-candidate.csv",
  profile = "tabular-data-resource",
  format = "csv",
  mediatype = "text/csv",
  encoding = "utf-8",
  description = sprintf(
    "Synthesised best-candidate cross-reference table linking all %s vegetation associations from statewide mapping units to IUCN GET Level 3 Ecosystem Functional Groups from Round %s expert elicitation.",
    n_distinct(xref_best_tbl$`System-Association Code`),
    the_round
  ),
  schema = list(
    primaryKey = "System-Association Code",
    fields = data.frame(
      name = names(dtstr),
      type = unname(dtstr),
      description = ""
    )
  )
)
write_csv(
  xref_best_tbl,
  here::here(out_dir, rs[[1]][['path']])
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

ctab <- efg_info |>
  filter(EFG %in% DPIRD_xy_best$EFG) |>
  transmute(value = efg_pixel_value, color = efg_hex_code)
coltab(target_raster) <- ctab

out_file <- here::here(out_dir, "DPIRD-xref-target.tiff")

rast_metadata <- c(
  PROJECT = "Ecosystem Indicators Workflows",
  TITLE = "Best candidate ecosystem function group",
  DESCRIPTION = "Synthesis of crossreference from DPIRD 006 to IUCN GET"
)
metags(target_raster) <- rast_metadata
raster_attributes <- efg_lookup_tbl |>
  transmute(
    id = coalesce(efg_pixel_value, 255L),
    label = paste(EFG, efg_name, sep = " ")
  )
levels(target_raster) <- raster_attributes
writeRaster(target_raster, out_file, overwrite = TRUE)

# out_file <- here::here(out_dir, "DPIRD-xref-target.tiff.vat.dbf")
# write.dbf(data.frame(raster_attributes), file = out_file)

## Metadata

codemeta <- read_json(here::here("scripts/codemeta.json"))
metadata <- read_json(here::here("results/template-zenodo.json"))
datapkg <- read_json(here::here("results/template-datapackage.json"))


metadata$title <- datapkg$title <- "Cross-referencing Wester Australia's ... to the IUCN Global Ecosystem Typology: 90m Best-Candidate Spatial Synthesis"

datapkg$name <- ""
datapkg$description <- ""


datapkg$created <- metadata$publication_date <- format(
  Sys.Date(),
  format = "%Y-%m-%d"
)

total_experts <- n_distinct(DPIRD_clean[[attr(DPIRD_clean, "interpreter")]])
how_many_units <- n_distinct(DPIRD_LUT[[attr(DPIRD_LUT, "id")]])
nr_eligible <- n_distinct(DPIRD_clean[[attr(DPIRD_clean, "source_id")]])
nr_targets <- n_distinct(DPIRD_clean[[attr(DPIRD_clean, "target_id")]])
total_rounds <- n_distinct(DPIRD_clean[[attr(DPIRD_clean, "round")]])

metadata$description <-
  sprintf(
    "<p>This dataset provides a 90 m spatial synthesis and tabular lookup cross-referencing Western Australia's vegetation associations from a statewide vegetation map (<strong>DPIRD 006</strong>; %s vegetation associations across Western Australia) to Level 3 Ecosystem Functional Groups (EFGs) of the <strong>IUCN Global Ecosystem Typology (IUCN GET)</strong>. </p>

<p>Cross-referencing relationships were established by %s domain experts across %s rounds of structured expert elicitation using fuzzy membership values. This package provides the synthesised <strong>best-candidate ('winner-takes-all')</strong> spatial layers from Round %s (%s eligible vegetation associations units mapped to %s IUCN GET Ecosystem Functional Groups, along with pixel-level membership proportions, number of supporting interpreters, tabular cross-reference tables, and GIS styling files.</p>

<p><strong>Attribution requirement:</strong> Spatial layers are derived from pre-European vegetation mapping of Western Australia (DPIRD-006) from <a href=\"https://catalogue.data.wa.gov.au\">dataWA</a> &copy;State of Western Australia, Department of Primary Industries and Regional Development. (CC BY 4.0).</p>",
    how_many_units,
    total_experts,
    total_rounds,
    the_round,
    nr_eligible,
    nr_targets
  )


kwds <- metadata$keywords
metadata$keywords <- c(unlist(kwds), "Western Australia")

rIds <- metadata$related_identifiers

datapkg$homepage <- codemeta$codeRepository

lics <- datapkg$licenses


lics[[length(lics) + 1]] <-
  list(
    name = "CC-BY-4.0",
    title = "Creative Commons Attribution 4.0 (Source DPIRD-006 data &copy;State of Western Australia, Department of Primary Industries and Regional Development)",
    path = "https://creativecommons.org/licenses/by/4.0/"
  )
datapkg$licenses <- lics

bbx <- st_bbox(target_raster)
dimensions <- list(
  rows = nrow(target_raster),
  columns = ncol(target_raster),
  bands = nlyr(target_raster)
)
datapkg$spatial <-
  c(
    crs = epsg_code,
    crs_name = st_crs(target_raster)[['input']],
    bbox = list(as.vector(bbx)),
    resolution_meters = list(res(target_raster)),
    dimensions = dimensions
  )

datapkg$resources <- rs

rIds[[length(rIds) + 1]] <-
  list(
    "identifier" = codemeta$codeRepository,
    relation = "isDerivedFrom",
    resource_type = "software",
    scheme = "url"
  )
rIds[[length(rIds) + 1]] <-
  list(
    "identifier" = "https://catalogue.data.wa.gov.au/en/dataset/pre-european-dpird-006",
    relation = "isDerivedFrom",
    resource_type = "dataset",
    scheme = "url"
  )
metadata$related_identifiers <- rIds

note_str <- metadata$notes
metadata$notes <- sprintf(
  note_str,
  "pre-European vegetation mapping of Western Australia (DPIRD-006) from dataWA &copy;State of Western Australia, Department of Primary Industries and Regional Development (CC BY 4.0)."
)
write_json(
  metadata,
  here::here(out_dir, ".zenodo.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

write_json(
  datapkg,
  here::here(out_dir, "datapackage.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
#  {
#       "name": "tasveg-xref-best-candidate",
#       "path": "TASVEG-xref-best-candidate.csv",
#       "profile": "tabular-data-resource",
#       "format": "csv",
#       "mediatype": "text/csv",
#       "encoding": "utf-8",
#       "description": "Synthesised best-candidate cross-reference table linking all 156 TASVEG 5.0 statewide mapping units (153 eligible vegetation communities and 3 non-eligible natural units) to IUCN GET Level 3 Ecosystem Functional Groups from Round 2 expert elicitation.",
#       "schema": {
#         "primaryKey": "VEGCODE",
#         "fields": [
#           {
#             "name": "VEG_GROUP",
#             "type": "string",
#             "description": "Broad TASVEG 5.0 vegetation group name."
#           },
#           {
#             "name": "VEGCODE",
#             "type": "string",
#             "description": "Three-letter TASVEG 5.0 mapping unit code."
#           },
#           {
#             "name": "VEGCODE_D",
#             "type": "string",
#             "description": "Full description of the TASVEG 5.0 vegetation community or mapping unit."
#           },
#           {
#             "name": "eligible",
#             "type": "boolean",
#             "description": "Whether the TASVEG 5.0 mapping unit is conceptually eligible for cross-referencing to a single ecosystem concept in IUCN GET (FALSE for OAQ, ORO, OSM)."
#           },
#           {
#             "name": "EFG",
#             "type": "string",
#             "description": "Best-candidate IUCN Global Ecosystem Typology Level 3 Ecosystem Functional Group code (e.g., T2.5)."
#           },
#           {
#             "name": "efg_name",
#             "type": "string",
#             "description": "Full name of the best-candidate IUCN GET Ecosystem Functional Group."
#           },
#           {
#             "name": "efg_pixel_value",
#             "type": "integer",
#             "description": "Standard integer pixel value used in TASVEG-xref-target.tiff and the Global Ecosystems Atlas (geco package)."
#           },
#           {
#             "name": "efg_hex_code",
#             "type": "string",
#             "description": "Standard hexadecimal color code (#RRGGBB) for the EFG from the geco package."
#           },
#           {
#             "name": "efg_rgb_code",
#             "type": "string",
#             "description": "Comma-separated RGB color values (R,G,B) for the EFG."
#           },
#           {
#             "name": "round",
#             "type": "integer",
#             "description": "Expert elicitation round number used for synthesis (2 = revised assessment after group discussion)."
#           },
#           {
#             "name": "prop_memb",
#             "type": "number",
#             "description": "Proportion of total Round 2 membership votes across all interpreters assigned to the best-candidate EFG (0 to 1)."
#           },
#           {
#             "name": "total_memb",
#             "type": "number",
#             "description": "Sum of membership values across all interpreters for the source VEGCODE in Round 2 (equals 5 when all 5 interpreters assessed the unit with weights summing to 1)."
#           },
#           {
#             "name": "supporting",
#             "type": "integer",
#             "description": "Number of interpreters in Round 2 who assigned a non-zero membership (> 0) to the best-candidate EFG (range: 2 to 5)."
#           },
#           {
#             "name": "total_interp",
#             "type": "integer",
#             "description": "Total number of interpreters who assessed the source VEGCODE in Round 2 (5)."
#           }
#         ]
#       }
#     },
#     {
#       "name": "iucn-get-pixel-lookup",
#       "path": "IUCN-GET-pixel-lookup.csv",
#       "profile": "tabular-data-resource",
#       "format": "csv",
#       "mediatype": "text/csv",
#       "encoding": "utf-8",
#       "description": "Lookup table for the 21 IUCN Global Ecosystem Typology Level 3 Ecosystem Functional Groups mapped as best candidates in Tasmania, including integer raster pixel values, realm/biome hierarchy, and color codes.",
#       "schema": {
#         "primaryKey": "efg_pixel_value",
#         "fields": [
#           {
#             "name": "efg_pixel_value",
#             "type": "integer",
#             "description": "Integer pixel value encoded in TASVEG-xref-target.tiff (9 to 170)."
#           },
#           {
#             "name": "EFG",
#             "type": "string",
#             "description": "IUCN GET Level 3 Ecosystem Functional Group code (e.g., T2.3, TF1.6)."
#           },
#           {
#             "name": "efg_name",
#             "type": "string",
#             "description": "Full name of the Ecosystem Functional Group."
#           },
#           {
#             "name": "realm_id",
#             "type": "string",
#             "description": "IUCN GET Level 1 Realm code (T, TF, F, MT, MFT)."
#           },
#           {
#             "name": "realm_name",
#             "type": "string",
#             "description": "IUCN GET Level 1 Realm name."
#           },
#           {
#             "name": "biome_id",
#             "type": "string",
#             "description": "IUCN GET Level 2 Functional Biome code."
#           },
#           {
#             "name": "biome_name",
#             "type": "string",
#             "description": "IUCN GET Level 2 Functional Biome name."
#           },
#           {
#             "name": "efg_hex_code",
#             "type": "string",
#             "description": "Standard hexadecimal color code (#RRGGBB)."
#           },
#           {
#             "name": "efg_rgb_code",
#             "type": "string",
#             "description": "Comma-separated RGB color components (R,G,B)."
#           },
#           {
#             "name": "n_tasveg_units",
#             "type": "integer",
#             "description": "Number of TASVEG 5.0 mapping units assigned to this EFG as their best candidate."
#           }
#         ]
#       }
#     },
#     {
#       "name": "tasveg-xref-target",
#       "path": "TASVEG-xref-target.tiff",
#       "format": "GeoTIFF",
#       "mediatype": "image/tiff; application=geotiff",
#       "description": "90 m raster of the best-candidate IUCN GET Ecosystem Functional Group (integer pixel values 9 to 170 matching IUCN-GET-pixel-lookup.csv, with embedded color table) in EPSG:28355."
#     },
#     {
#       "name": "tasveg-xref-membership",
#       "path": "TASVEG-xref-membership.tiff",
#       "format": "GeoTIFF",
#       "mediatype": "image/tiff; application=geotiff",
#       "description": "90 m floating-point raster of the proportion of total membership votes (prop_memb, range 0.33 to 1.00) supporting the best-candidate EFG in EPSG:28355."
#     },
#     {
#       "name": "tasveg-xref-supporting",
#       "path": "TASVEG-xref-supporting.tiff",
#       "format": "GeoTIFF",
#       "mediatype": "image/tiff; application=geotiff",
#       "description": "90 m raster of the number of interpreters (supporting, range 2 to 5 out of 5) assigning non-zero membership to the best-candidate EFG in EPSG:28355."
#     },
#     {
#       "name": "tasveg-xref-target-qml",
#       "path": "TASVEG-xref-target.qml",
#       "format": "QML",
#       "mediatype": "application/xml",
#       "description": "QGIS layer style file providing automatic paletted rendering and EFG code/name legend labels for TASVEG-xref-target.tiff."
#     },
#     {
#       "name": "tasveg-xref-target-clr",
#       "path": "TASVEG-xref-target.clr",
#       "format": "CLR",
#       "mediatype": "text/plain",
#       "description": "GDAL / ArcGIS RGBA color map file for TASVEG-xref-target.tiff."
#     }
