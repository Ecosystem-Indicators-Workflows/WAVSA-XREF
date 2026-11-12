here::i_am("scripts/00-data-download.R")


# Level 6 classification: Pre-European Vegetation (DPIRD-006) with brief descriptions found in Pre-European Veg Look Up Table (LUT) Summary

# <>


#lut_url <- "https://catalogue.data.wa.gov.au/dataset/90bc5622-bc12-4d0a-8b49-6be27dcf0e91/resource/d6ce041c-8f7a-4309-9f52-44c1b617a4c8/download/preeuropeanvegetationlutsummary-may2012.xlsx"

output_dir <- here::here("data/source-data")
if (!dir.exists(output_dir))
  dir.create(output_dir)

lut_file <- here::here(output_dir, "preeuropeanvegetationlutsummary-may2012.xlsx")
LUT <- read_xls(lut_file)


#<https://catalogue.data.wa.gov.au/en/dataset/pre-european-dpird-006>

data_url <- "https://direct-download.slip.wa.gov.au/datadownload/DPIRD_Public/Pre_European_Vegetation_DPIRD_006_WA_GDA2020_Public_Geopackage.zip"

#This link is for scripting and requires token authentication. For more information see the Data WA Help Centre article.

#> The pre-European vegetation mapping of Western Australia dataset is an output of a joint project. It maps original natural vegetation presumed to have existed prior to European settlement in Western Australia. Descriptions of each of the vegetation types can be found in the accompanying memoir.

#> The major sources of data in this database are the published and unpublished mapping of J.S. Beard at 1:250,000 scale. There are c 30,000 polygons covering 160 1:250,000 map sheets. Data on the original vegetation of all of Western Australia, with the exception of three map sheets in the south-west corner, were captured from J S Beard’s original working drawings, where these were available, or from published maps, all at the scale of 1:250,000. For the three map sheets in the south-west corner, a new data set was compiled in a form consistent with Beard’s approach, from existing data (A.J.M. Hopkins, unpublished).

#> Also published as Beard, J. S., Beeston, G.R., Harvey, J.M., Hopkins, A. J. M. and Shepherd, D. P. 2013. The vegetation of Western Australia at the 1:3,000,000 scale. Explanatory memoir. Second edition. Conservation Science Western Australia 9: 1-152.
# classification unit is Vegetation System-Association

# number of units: 1567 System-Association codes; 1620 Map Unit iDs

# Spatial layer: Pre-European Vegetation (DPIRD-006),,,,,,
# Spatial layer url: https://data-downloads.slip.wa.gov.au/DPIRD-006/Shapefile

# join_field,sa_code

