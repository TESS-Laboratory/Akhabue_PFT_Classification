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
# Count the occurrences of each species
species_count <- Trait_species %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the total number of unique species
print(nrow(species_count))



