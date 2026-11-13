## Setup

### Libraries

# #### tidyverse libraries
library(dplyr)
library(readr)

# #### Read and write other formats
library(jsonlite)

# #### Spatial data
library(terra)

### Paths
here::i_am("scripts/11-create-metadata-best-candidate.R")

out_dir <- here::here("results", "output1-best-candidate-raster")
if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
}

## Metadata

## Get data from here:
codemeta <- read_json(here::here("scripts/codemeta.json"))

## Update these:
metadata <- read_json(here::here("results/template-zenodo.json"))
datapkg <- read_json(here::here("results/template-datapackage.json"))

## Write here:

metadata_out <- here::here(out_dir, ".zenodo.json")
datapkg_out <- here::here(out_dir, "datapackage.json")

## Input files

xref_best_file <- "DPIRD-xref-best-candidate.csv"
xref_best_tbl <- read_csv(here::here(out_dir, xref_best_file))

efg_lookup_file <- "IUCN-GET-pixel-lookup.csv"
efg_lookup_tbl <- read_csv(here::here(out_dir, efg_lookup_file))


xref_tiff_file <- "DPIRD-xref-target.tiff"
r_target <- rast(file.path(out_dir, xref_tiff_file))

memb_tiff_file <- "DPIRD-xref-membership.tiff"
supp_tiff_file <- "DPIRD-xref-supporting.tiff"

xref_clr_file <- "DPIRD-xref-target.clr"
xref_aux_file <- "DPIRD-xref-target.tiff.aux.xml"
xref_qml_file <- "DPIRD-xref-target.qml"

## Resources for the datapackage

the_round <- unique(xref_best_tbl$round)
total_experts <- max(xref_best_tbl$total_interp)
min_memb <- min(xref_best_tbl$total_memb)
max_memb <- max(xref_best_tbl$total_memb)
min_supporting <- min(xref_best_tbl$supporting)
max_supporting <- max(xref_best_tbl$supporting)
min_prop <- min(xref_best_tbl$prop_memb)
max_prop <- max(xref_best_tbl$prop_memb)


## Need to check this independently...
how_many_units <- n_distinct(xref_best_tbl$`System-Association Code`)
total_rounds <- max(the_round)
nr_eligible <- how_many_units
nr_targets <- n_distinct(xref_best_tbl$EFG)
#
# how_many_units <- n_distinct(DPIRD_LUT[[attr(DPIRD_LUT, "id")]])
# nr_eligible <- n_distinct(DPIRD_clean[[attr(DPIRD_clean, "source_id")]])
# nr_targets <- n_distinct(DPIRD_clean[[attr(DPIRD_clean, "target_id")]])
# total_rounds <- n_distinct(DPIRD_clean[[attr(DPIRD_clean, "round")]])

# Spatial grid statistics
crs_info <- crs(r_target, describe = TRUE)
crs_label <- sprintf(
    "%s (%s:%s)",
    crs_info$name[1],
    crs_info$authority[1],
    crs_info$code[1]
)
res_xy <- res(r_target)
pixel_km2 <- prod(res_xy) / 1e6
pixel_ha <- prod(res_xy) / 1e4
n_rows <- nrow(r_target)
n_cols <- ncol(r_target)
n_bands <- nlyr(r_target)
total_cells <- ncell(r_target)
ext_bbox <- as.vector(ext(r_target)) # xmin, xmax, ymin, ymax

dimensions <- list(
    rows = n_rows,
    columns = n_cols,
    bands = n_bands
)
datapkg$spatial <-
    c(
        crs = paste(crs_info[2:3], collapse = ":"),
        crs_name = crs_info$name,
        bbox = list(ext_bbox),
        resolution_meters = list(res_xy),
        dimensions = dimensions
    )


## resources for data package

rs <- list()

dtstr <- sapply(xref_best_tbl, class)
names <- names(dtstr)
types <- unname(dtstr)
descriptions <- c(
    "System association code for the source units.",
    "Description of the system association unit.",
    "Whether the source unit is conceptually eligible for cross-referencing to an ecosystem concept in the IUCN GET.",
    "Best-candidate IUCN Global Ecosystem Typology Level 3 Ecosystem Functional Group code (e.g., T2.5).",
    "Full name of the best-candidate IUCN GET Ecosystem Functional Group.",
    "Standard integer pixel value used in DPIRD-xref-target.tiff and the Global Ecosystems Atlas (geco package).",
    "Standard hexadecimal color code (#RRGGBB) for the EFG from the geco package.",
    "Comma-separated RGB color values (R,G,B) for the EFG.",
    sprintf(
        "Expert elicitation round number used for synthesis (%s = revised assessment after group discussion).",
        the_round
    ),
    sprintf(
        "Proportion of total Round %s membership values across all interpreters assigned to the best-candidate EFG (0 to 1).",
        the_round
    ),
    sprintf(
        "Sum of membership values across all interpreters for the source unit in Round %s (data range: %s to %s indicates errors, should be equals %s when all %s interpreters assessed the unit with weights summing to 1).",
        the_round,
        min_memb,
        max_memb,
        total_experts * 1.0,
        total_experts
    ),
    sprintf(
        "Number of interpreters in Round %s who assigned a non-zero membership (> 0) to the best-candidate EFG (range: %s to %s).",
        the_round,
        min_supporting,
        max_supporting
    ),
    sprintf(
        "Total number of interpreters who assessed the source unit in Round %s (%s).",
        the_round,
        total_experts
    )
)

field_items <- list()
for (j in 1:length(names)) {
    field_items[[j]] <- list(
        name = names[j],
        type = types[j],
        description = descriptions[j]
    )
}

rs[[1]] <- list(
    name = "Crossreference summary",
    path = xref_best_file,
    profile = "tabular-data-resource",
    format = "csv",
    mediatype = "text/csv",
    encoding = guess_encoding(here::here(out_dir, xref_best_file))$encoding,
    description = sprintf(
        "Synthesised best-candidate cross-reference table linking all %s vegetation associations from statewide mapping units to IUCN GET Level 3 Ecosystem Functional Groups from Round %s expert elicitation.",
        n_distinct(xref_best_tbl$`System-Association Code`),
        unique(xref_best_tbl$round)
    ),
    schema = list(
        primaryKey = "System-Association Code",
        fields = field_items
    )
)

min_pixel <- min(efg_lookup_tbl$efg_pixel_value)
max_pixel <- max(efg_lookup_tbl$efg_pixel_value)

dtstr <- sapply(efg_lookup_tbl, class)
names <- names(dtstr)
types <- unname(dtstr)
descriptions <- c(
    sprintf(
        "Integer pixel value encoded in %s (%s to %s).",
        xref_tiff_file,
        min_pixel,
        max_pixel
    ),
    "IUCN GET Level 3 Ecosystem Functional Group code (e.g., T2.3, TF1.6).",
    "Full name of the Ecosystem Functional Group.",
    "IUCN GET Level 1 Realm code (T, TF, F, MT, MFT).",
    "IUCN GET Level 1 Realm name.",
    "IUCN GET Level 2 Functional Biome code.",
    "IUCN GET Level 2 Functional Biome name.",
    "Standard hexadecimal color code (#RRGGBB).",
    "Comma-separated RGB color components (R,G,B).",
    "Number of source units assigned to this EFG as their best candidate."
)

field_items <- list()
for (j in 1:length(names)) {
    field_items[[j]] <- list(
        name = names[j],
        type = types[j],
        description = descriptions[j]
    )
}

rs[[2]] <- list(
    name = "Pixel Lookup table",
    path = efg_lookup_file,
    profile = "tabular-data-resource",
    format = "csv",
    mediatype = "text/csv",
    encoding = guess_encoding(here::here(out_dir, efg_lookup_file))$encoding,
    description = sprintf(
        "Lookup table for the %s IUCN Global Ecosystem Typology Level 3 Ecosystem Functional Groups mapped as best candidates in Western Australia, including integer raster pixel values, realm/biome hierarchy, and color codes.",
        n_distinct(efg_lookup_tbl$EFG)
    ),
    schema = list(
        primaryKey = "efg_pixel_value",
        fields = field_items
    )
)


rs[[3]] <- list(
    name = "Best candidate EFG raster",
    path = xref_tiff_file,
    format = "GeoTIFF",
    mediatype = "image/tiff; application=geotiff",
    description = sprintf(
        "90 m raster of the best-candidate IUCN GET Ecosystem Functional Group (integer pixel values %s to %s matching %s, with embedded color table) in %s.",
        min_pixel,
        max_pixel,
        efg_lookup_file,
        crs_label
    )
)


rs[[4]] <- list(
    name = "Best candidate EFG sidecar file",
    path = xref_aux_file,
    format = "XML",
    mediatype = "application/xml",
    description = sprintf(
        "Persistent Auxiliary Metadata (PAM) with categorical information for %s.",
        xref_tiff_file
    )
)

rs[[5]] <- list(
    name = "Best candidate EFG colour file",
    path = xref_clr_file,
    format = "CLR",
    mediatype = "text/plain",
    description = sprintf(
        "GDAL / ArcGIS RGBA color map file for %s.",
        xref_tiff_file
    )
)

rs[[6]] <- list(
    name = "Best candidate EFG style file",
    path = xref_qml_file,
    format = "QML",
    mediatype = "application/xml",
    description = sprintf(
        "QGIS layer style file providing automatic paletted rendering and EFG code/name legend labels for %s.",
        xref_tiff_file
    )
)


rs[[7]] <- list(
    name = "Best candidate membership raster",
    path = memb_tiff_file,
    format = "GeoTIFF",
    mediatype = "image/tiff; application=geotiff",
    description = sprintf(
        "90 m floating-point raster of the proportion of total membership votes (prop_memb, range %s to %s) supporting the best-candidate EFG in %s.",
        min_prop,
        max_prop,
        crs_label
    )
)

rs[[8]] <- list(
    name = "Best candidate support raster",
    path = supp_tiff_file,
    format = "GeoTIFF",
    mediatype = "image/tiff; application=geotiff",
    description = sprintf(
        "90 m raster of the number of interpreters (supporting, range %s to %s out of %s) assigning non-zero membership to the best-candidate EFG in %s.",
        min_supporting,
        max_supporting,
        total_experts,
        crs_label
    )
)
# #### Best candidate

metadata$title <- datapkg$title <- "Cross-referencing Western Australia's vegetation associations to the IUCN Global Ecosystem Typology: 90m Best-Candidate Spatial Synthesis"

datapkg$name <- "dpird-006-xref-iucn-get-90m-best-candidate"
datapkg$description <- sprintf(
    "Spatial synthesis (90 m GeoTIFF rasters) and tabular lookup tables cross-referencing DPIRD 006 vegetation association systems of Western Australia to Level 3 Ecosystem Functional Groups (EFGs) of the IUCN Global Ecosystem Typology based on Round %s structured expert elicitation across %s interpreters.",
    the_round,
    xfun::numbers_to_words(total_experts)
)
datapkg$contributors <- metadata$creators
datapkg$created <- metadata$publication_date <- format(
    Sys.Date(),
    format = "%Y-%m-%d"
)


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
        title = "Source DPIRD-006 data &copy;State of Western Australia, Department of Primary Industries and Regional Development",
        path = "https://creativecommons.org/licenses/by/4.0/"
    )
datapkg$licenses <- lics


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

metadata$version <- datapkg$version <- codemeta$version

write_json(
    metadata,
    metadata_out,
    auto_unbox = TRUE,
    pretty = TRUE
)

write_json(
    datapkg,
    datapkg_out,
    auto_unbox = TRUE,
    pretty = TRUE
)
