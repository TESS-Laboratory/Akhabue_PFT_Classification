# Pipeline----
# read in lookup data for categorical traits
# clean the data by removing the columns you don't need.
# read in trait data
# sort by species, to determine the species you have and how many species your observation covers


{ # list out the different PFTs in JULES
# BET-Tr (tropical broadleaf evergreen trees), 
# BET-Te (temperate broadleaf evergreen trees), 
# BDT (broadleaf deciduous trees), 
# NET (needle-leaf evergreen trees), 
# NDT (needle-leaf deciduous trees), 
# ESh (evergreen shrubs), 
# DSh (deciduous shrubs),
# C3 grasses, 
# C4 grasses, 
}

# Assign each specie a PFT class based on the information from the lookup table
# sort the assigned species according to the species from your work data - as these are the species you will be working with
# merge the new df - the pft classification data - to match with your working data 

# OR
# Ask Anna for her classification table and do the last bit above. - if the lookup data hasn't changed since the last time she did it.







#Code----------------------------------------------------------------------------

# Load packages ----
library(dplyr)

# Read the trait data file and the look up table ----
Trait_species <- read.csv("workdata_traits.csv")
Categorical_table <- read.csv("TRY_Categorical_Traits_Lookup_Table.csv")

# Keep only the columns we need for the categorical look up table
vars <- c("AccSpeciesID", "AccSpeciesName", "Genus", "SpeciesEpithet", "Family", "PlantGrowthForm", "LeafType",
          "LeafPhenology", "PhotosyntheticPathway")

Categorical_table<- Categorical_table %>% dplyr::select(one_of(vars))


# Sort the unique species ----
# Count the occurrences of each species from my observation
species_count <- Trait_species %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the total number of unique species
print(nrow(species_count))


# Sort unique values for the other variables in the categorical table eg leaf type, plant growth form, leaf phenology, photosynthetic pathway
# this is to know the different variables and their individual count
# this is also import for QC to help indicate for duplicates and solve that problem

species_count_TRY <- Categorical_table %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

PlantGrowthForm <- Categorical_table %>%
  group_by(PlantGrowthForm) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

LeafType <- Categorical_table %>%
  group_by(LeafType) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

LeafPhenology <- Categorical_table %>%
  group_by(LeafPhenology) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

PhotosyntheticPathway <- Categorical_table %>%
  group_by(PhotosyntheticPathway) %>%
  summarise(count = n()) %>%
  arrange(desc(count))




# group species into different PFT classes
# At this stage I will combine theBET-Tr (tropical broadleaf evergreen trees), and
# BET-Te (temperate broadleaf evergreen trees) together. Because I will need the GeoLoc to group into temp and tropical

# Create a new df, sorting out based on the following criteria LeafType, PlantGrowtForm, LeafPhenology, PhotosynthicPathway (This may not be essential for all the PFT classes; just the grasses)