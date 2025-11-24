# Installing and loading the rtry package ----

install.packages('rtry')
install.packages('dplyr')
install.packages(c("data.table", "tidyr", "jsonlite", "curl"))

library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
library(plotrix)
library(sf)
library(rnaturalearth)
library(lwgeom)
library(maps)
library(RColorBrewer) # not using this (atm)
library(tidyr)
library(tidyverse)
library(patchwork)
library(viridis)
library(rgeos)

library(rtry)
packageVersion('rtry')

# Importing and exploring dataset ----

TRYdata1 <- rtry_import("~/OneDrive - University of Exeter/Desktop/O3-/TRY DataFile/29791.txt")
TRYdata2 <- rtry_import("~/OneDrive - University of Exeter/Desktop/O3-/TRY DataFile/32833.txt")
TRYdata3 <- rtry_import("~/OneDrive - University of Exeter/Desktop/O3-/TRY DataFile/35747.txt")
TRYdata4 <- rtry_import("~/OneDrive - University of Exeter/Desktop/O3-/TRY DataFile/35746.txt")
TRYdata5 <- rtry_import("~/OneDrive - University of Exeter/Desktop/O3-/TRY DataFile/35745.txt")


TRYdata <- do.call(
  rtry_bind_row,
  lapply(list(TRYdata1, TRYdata2, TRYdata3, TRYdata4, TRYdata5),
         function(x) { x$StdValueStr <- as.character(x$StdValueStr); x })
)



# Group the input data based on TraitID and TraitName
TRYdata_explore_trait <- rtry_explore(TRYdata, TraitID, TraitName)

# Group the input data based on AccSpeciesID, AccSpeciesName, TraitID and TraitName
# Note: For TraitID == "NA", meaning that entry is an ancillary data
TRYdata_explore_species <- rtry_explore(TRYdata, AccSpeciesID, AccSpeciesName, TraitID, TraitName)

# Group the input data based on DataID, DataName, TraitID and TraitName
# Then sort the output by TraitID using the sortBy argument
TRYdata_explore_anc <- rtry_explore(TRYdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)


# Keeping columns and rows ----
workdata <- rtry_remove_col(TRYdata, V29)

workdata <- rtry_select_col(TRYdata, ObsDataID, ObservationID, AccSpeciesID, AccSpeciesName, ValueKindName, TraitID, TraitName, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, UnitName, OrigObsDataID, ErrorRisk, Reference, Comment)

workdata <- rtry_select_row(workdata, TraitID > 0 | DataID %in% c(59, 60, 61, 6601, 327, 413, 1961, 113, 1863))

# It is recommended to backup the data at some stages of the preprocessing, best before excluding according to different attributes.
workdata_unexcluded <- workdata




# Georeference coding to exclude data with only lat and long ----
## Filter according to latitude ----
## First, obtain only the observations that contain the Latitude (DataID 59) information, i.e. geo-referenced observations, using the function rtry_select_row.----
# Select only the geo-referenced observations, i.e. with DataID 59 Latitude
# Set getAncillary to TRUE to obtain (keep) all traits and ancillary data

workdata <- rtry_select_row(workdata, DataID %in% 59, getAncillary = TRUE)


# # # # # # # # # # # # #
# Exclude observations using latitude information
# Criteria
# 1. DataID equals to 59
# 2. StdValue NA

workdata <- rtry_exclude(workdata, (DataID %in% 59) & (is.na(StdValue)), baseOn = ObservationID)


# Select only the geo-referenced observations with DataID 60 Longitude
# Set getAncillary to TRUE to obtain (keep) all traits and ancillary data
workdata <- rtry_select_row(workdata, DataID %in% 60, getAncillary = TRUE)


# Exclude observations using longitude information
# Criteria
# 1. DataID equals to 60
# 2. StdValue smaller than 10 or larger than 60 or NA
workdata <- rtry_exclude(workdata, (DataID %in% 60) & (is.na(StdValue)), baseOn = ObservationID)


# Remove duplicates
workdata <- rtry_remove_dup(workdata)



#transform to wide table
# Exclude
# 1. All entries with "" in TraitID
# 2. Potential categorical traits that don't have a StdValue
# 3. Traits that have not yet been standardized in TRY
# Then select the relevant columns for transformation
# Note: The complete.cases() is used to ensure the cases are complete,
#       i.e. have no missing values
num_traits <- rtry_select_row(workdata, complete.cases(TraitID) & complete.cases(StdValue))

num_traits <- rtry_select_col(num_traits, ObservationID, AccSpeciesID, AccSpeciesName, TraitID, TraitName, StdValue, UnitName, Reference)

# Extract the unique value of latitude (DataID 59) and longitude (DataID 60) together with the corresponding ObservationID
workdata_georef <- rtry_select_anc(workdata, 59, 60, 1863, 113, 327, 413)

# To merge the extracted ancillary data with the numerical traits
# Merge the relevant data frames based on the ObservationID using rtry_join_left (left join)
num_traits_georef <- rtry_join_left(num_traits, workdata_georef, baseOn = ObservationID)



# Perform wide table transformation on TraitID, TraitName and UnitName
# With cell values to be the mean values calculated for StdValue
num_traits_georef_wider <- rtry_trans_wider(num_traits_georef, names_from = c(TraitID, TraitName, UnitName), values_from = c(StdValue), values_fn = list(StdValue = mean))



#export data

# Export the data into a CSV file
output_file = file.path(tempdir(), "workdata_wider.csv")
rtry_export(num_traits_georef_wider, output_file)


# Export the data into a CSV file
output_file = file.path(tempdir(), "workdata_.csv")
rtry_export(num_traits_georef, output_file)








# Sorting Africa data and preparation  ----


workdata<- read_csv("workdata_.csv")


# Remove unknown species in the AccSpeciesName column
workdata <- workdata %>%
  filter(AccSpeciesName != "unknown")


# Filter out rows where StdValue is less than 0
workdata <- workdata %>%
  filter(StdValue > 0)


# Keep only the columns needed ----
vars <- c("AccSpeciesName", "TraitName", "UnitName", "Latitude", 
          "Longitude", "Exposition", "Plant_dev_status", "Reference")

workdata<- workdata %>% dplyr::select(one_of(vars))


# World map data in a Robinson projection
world <- ne_countries(scale = "medium", returnclass = "sf")
world_robinson <- st_transform(world, crs = "+proj=robin")


africa <- sf::st_make_valid(world[world$continent == "Africa", ])


# Filter trait data for Africa
africa_data <- workdata %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_filter(africa)


write.csv(africa_data, "africa_spp_data.csv", row.names = FALSE)



