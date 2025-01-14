# Load packages ----
library(tidyverse)
library(viridis)  # For color scales
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# Read and clean data file  ----

Mapped_PFT <- read.csv("Mapped_PFT_data.csv")


TaxonomicHarmonization <- read.csv("TaxonomicHarmonization.csv")




# Merge the datasets based on AccSpeciesName
merged_data <- merge(Mapped_PFT, TaxonomicHarmonization, by = "AccSpeciesName", all.x = TRUE)

# The resulting merged_data will contain all rows from Mapped_PFT_data and only the matching rows from TaxonomicHarmonization
# to keep only the species that are found in both datasets, use inner join

matched_data <- merge(Mapped_PFT, TaxonomicHarmonization, by = "AccSpeciesName")

