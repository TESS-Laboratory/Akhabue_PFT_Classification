# Pipeline----
#  ✓ read in lookup data for categorical traits
#  ✓ clean the data by removing the columns you don't need.
#  ✓ read in trait data
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









#Code----------------------------------------------------------------------------

# Load packages ----
library(tidyverse)
library(dplyr)

# Read the trait data file and the look up table ----
Trait_species <- read.csv("trait_africa.csv")


# Look up table
Categorical_table <- read.csv("TRY_Categorical_Traits_Lookup_Table.csv")

# Keep only the columns we need for the categorical look up table
vars <- c("AccSpeciesID", "AccSpeciesName", "Genus", "SpeciesEpithet", "Family", "PlantGrowthForm", "LeafType",
          "LeafPhenology", "PhotosyntheticPathway")

Categorical_table<- Categorical_table %>% dplyr::select(one_of(vars))




# Sort the unique species from my working data ----
# Count the occurrences of each species from my observation
species_count_traitdata <- Trait_species %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the total number of unique species
print(nrow(species_count_traitdata))


# Merge the datasets based on AccSpeciesName
merged_data <- merge(species_count_traitdata, Categorical_table, by = "AccSpeciesName", all.x = TRUE)

# The resulting merged_data will contain all rows from species_count_traitdata and only the matching rows from categorical_table
# If you want to keep only the species that are found in both datasets, you can use inner join

matched_data <- merge(species_count_traitdata, Categorical_table, by = "AccSpeciesName")


# Sort unique values for the other variables in the categorical table eg leaf type, plant growth form, leaf phenology, photosynthetic pathway
# this is to know the different variables and their individual count
# this is also important for QC to help indicate for duplicates and solve that problem

species_count_TRY <- Categorical_table %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

PlantGrowthForm <- matched_data %>%
  group_by(PlantGrowthForm) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



matched_data$LeafType[which(matched_data$LeafType == "")] <- "broadleaved"

LeafType <- matched_data %>%
  group_by(LeafType) %>%
  summarise(count = n()) %>%
  arrange(desc(count))




species_to_update1 <- c("Baikiaea plurijuga", "Brachystegia longifolia", "Brachystegia spiciformis", 
                        "Bridelia ferruginea", "Cassia sieberiana", "Combretum glutinosum", "Combretum micranthum", 
                        "Combretum nigricans", "Crossopteryx febrifuga", "Detarium microcarpum", "Dioscorea bulbifera", 
                        "Faidherbia albida", "Flueggea virosa", "Gmelina arborea", "Lannea velutina", 
                        "Lonchocarpus laxiflorus", "Lophira lanceolata", "Milicia excelsa", "Peltophorum africanum", 
                        "Prosopis africana", "Quercus faginea", "Sterculia tragacantha", "Ximenia americana", "Albizia chinensis")

matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName %in% species_to_update1 & LeafPhenology == "deciduous/evergreen", 
                                "deciduous", LeafPhenology))


species_to_update2 <- c("Aspidosperma megalocarpon", "Bauhinia petersiana", "Berchemia zeyheri", "Carapa procera", 
                        "Crassula rupestris", "Gardenia ternifolia", "Isoberlinia doka", "Neoboutonia macrocalyx", 
                        "Poulsenia armata", "Schotia afra", "Scolopia zeyheri", "Strychnos pungens", "Cordia megalantha")


matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName %in% species_to_update2 & LeafPhenology == "deciduous/evergreen", 
                                "evergreen", LeafPhenology))


matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName == "Strophanthus sarmentosus" & LeafPhenology == "evergreen", 
                                "deciduous", LeafPhenology))



LeafPhenology <- matched_data %>%
  group_by(LeafPhenology) %>%
  summarise(count = n()) %>%
  arrange(desc(count))




matched_data <- matched_data %>%
  mutate(PhotosyntheticPathway = ifelse(AccSpeciesName =="Chenopodium album" & PhotosyntheticPathway == "C3/C4",
                                        "C3", PhotosyntheticPathway))


matched_data <- matched_data %>%
  mutate(PhotosyntheticPathway = ifelse(AccSpeciesName =="Euphorbia hirta" & PhotosyntheticPathway == "C3/C4",
                                        "C4", PhotosyntheticPathway))


species_to_update5 <- c("Agrostis kilimandscharica", "Festuca abyssinica", "Poa leptoclada", "Ehrharta stipoides", 
                        "Isachne mauritiana", "Pseudechinolaena polystachya", "Isachne mauritiana", "Poa cookii",
                        "Stipa parviflora", "Stipa tenacissima", "Pseudechinolaena polystachya", "Festuca obturbans",
                        "Actiniopteris radiata", "Asplenium friesiorum", "Dryopteris kilemensis", "Pteris catoptera",
                        "Selaginella kraussiana")

matched_data <- matched_data %>% 
  mutate (PhotosyntheticPathway = ifelse(AccSpeciesName %in% species_to_update5 & PhotosyntheticPathway == "", 
                                         "C3", PhotosyntheticPathway))


species_to_update6 <- c("Andropogon pinguipes", "Aristida adoensis", "Carex aethiopica", "Uncinia compacta", 
                        "Eragrostis patula",  "Cymbopogon pospischilii", "Digitaria abyssinica", 
                        "Digitaria pearsonii", "Tristachya biseriata", "Schmidtia kalahariensis", "Hyparrhenia confinis", 
                        "Chrysopogon plumulosus", "Chloris mossambicensis")

matched_data <- matched_data %>% 
  mutate (PhotosyntheticPathway = ifelse(AccSpeciesName %in% species_to_update6 & PhotosyntheticPathway == "", 
                                         "C4", PhotosyntheticPathway))

PhotosyntheticPathway <- matched_data %>%
  group_by(PhotosyntheticPathway) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# group species into different PFT classes
# At this stage I will combine theBET-Tr (tropical broadleaf evergreen trees), and
# BET-Te (temperate broadleaf evergreen trees) together. Because I will need the GeoLoc to group into temp and tropical
# Considering that all my data are from Africa this should be easy to sort out

# Create a new df, sorting out based on the following criteria LeafType, PlantGrowtForm, LeafPhenology, PhotosynthicPathway 
# (This may not be essential for all the PFT classes; just the grasses)



# this is for BET_Tr and BET_Tr---- 

# filter rows based on PlantGrowthForm that is tree or shrub/tree
PFT_Tree <- matched_data %>%
  filter(PlantGrowthForm %in% c("tree", 
                                "shrub/tree"))

# View the filtered data
print(PFT_Tree)


PFT_Tree_Broadleaf <- PFT_Tree %>%
  filter(LeafType %in% c("broadleaved"))


PFT_Tree_Evergreen <- PFT_Tree_Broadleaf %>%
  filter(LeafPhenology %in% c("evergreen"))                   # NO. 1+2 BET - Te+Tr


PFT_Tree_Evergreen$PFT <- 'BET'



# For BDT ----
PFT_Tree_Deciduous <- PFT_Tree_Broadleaf %>%
  filter(LeafPhenology %in% c("deciduous"))                   # NO. 3 BDT

PFT_Tree_Deciduous$PFT <- 'BDT'



# For NET (needle-leaf trees)----

PFT_Needleleaf <- PFT_Tree %>%
  filter(LeafType %in% c("scale-shaped",
                         "needleleaved"))


PFT_Needleleaf_Evergreen <- PFT_Needleleaf %>%
  filter(LeafPhenology %in% c("evergreen"))                   # NO. 4 NET     

PFT_Needleleaf_Evergreen$PFT <- 'NET'



PFT_Needleleaf_Deciduous <- PFT_Needleleaf %>%
  filter(LeafPhenology %in% c("deciduous"))                   # NO. 5 NDT


PFT_Needleleaf_Deciduous$PFT <- 'NDT'



# For shrubs ----
PFT_Shrub  <- matched_data %>%
  filter(PlantGrowthForm %in% c("shrub", "herb", "herb/shrub"))


PFT_Shrub_Evergreen <- PFT_Shrub %>%
  filter(LeafPhenology %in% c("evergreen"))                   # NO. 6 ESH

PFT_Shrub_Evergreen$PFT <- 'ESH'



PFT_Shrub_Deciduous <- PFT_Shrub  %>%
  filter(LeafPhenology %in% c("deciduous"))                   # NO. 7 DSH

PFT_Shrub_Deciduous$PFT <- 'DSH'



# for grasses ----
PFT_Grass <- matched_data %>%
  filter(PlantGrowthForm %in% c("graminoid", "fern"))



PFT_Grass_C3 <- PFT_Grass %>%
  filter(PhotosyntheticPathway %in% c("C3", "C3/CAM"))        # NO. 8 C3


PFT_Grass_C3$PFT <- 'C3'


PFT_Grass_C4 <- PFT_Grass %>%
  filter(PhotosyntheticPathway %in% c("C4", "C4/CAM"))        # NO. 9 C4

PFT_Grass_C4$PFT <- 'C4'



# add a new column to all of these new df and in put the PFT you have assigned,
# after which combine all the df.

# then you can start working on the other 54+k




# Combine all data frames into one
combined_df_PFT <- bind_rows(PFT_Tree_Evergreen, PFT_Tree_Deciduous, PFT_Needleleaf_Evergreen, 
                             PFT_Needleleaf_Deciduous, PFT_Shrub_Evergreen, PFT_Shrub_Deciduous, 
                             PFT_Grass_C3, PFT_Grass_C4)


# View the combined data frame
print(combined_df_PFT)



# the remainder from the growth form.

PFT_Ramnant <- matched_data %>%
  filter(PlantGrowthForm %in% c("herb", "fern"))

# combine this to the NA from PFT_Tree, NA from PFT_Tree_Broadleaf, NA from PFT_Needleleaf,
# NA from PFT_Shrub, NA from PFT_Grass

