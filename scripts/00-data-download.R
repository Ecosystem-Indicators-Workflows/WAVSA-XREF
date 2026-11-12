here::i_am("scripts/00-data-download.R")


# Level 6 classification: Pre-European Vegetation (DPIRD-006) with brief descriptions found in Pre-European Veg Look Up Table (LUT) Summary

# <>


# Manual download
#lut_url <- "https://catalogue.data.wa.gov.au/dataset/90bc5622-bc12-4d0a-8b49-6be27dcf0e91/resource/d6ce041c-8f7a-4309-9f52-44c1b617a4c8/download/preeuropeanvegetationlutsummary-may2012.xlsx"

output_dir <- here::here("data/source-data")
if (!dir.exists(output_dir))
  dir.create(output_dir)

dir(output_dir)

lut_file <- here::here(output_dir, "preeuropeanvegetationlutsummary-may2012.xlsx")
LUT <- read_xls(lut_file)


#<https://catalogue.data.wa.gov.au/en/dataset/pre-european-dpird-006>

#This link is for scripting and requires token authentication. For more information see the Data WA Help Centre article.
data_url <- "https://direct-download.slip.wa.gov.au/datadownload/DPIRD_Public/Pre_European_Vegetation_DPIRD_006_WA_GDA2020_Public_Geopackage.zip"



  ## Unzip the selected file
  unzip(
    here::here(output_dir, "Pre_European_Vegetation_DPIRD_006_WA_GDA2020_Public_Geopackage.zip"),
    exdir = output_dir
  )


# number of units: 1567 System-Association codes; 1620 Map Unit iDs

# Spatial layer: Pre-European Vegetation (DPIRD-006),,,,,,
# Spatial layer url: https://data-downloads.slip.wa.gov.au/DPIRD-006/Shapefile

# join_field,sa_code

