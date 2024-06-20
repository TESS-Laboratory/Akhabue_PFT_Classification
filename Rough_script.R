# Load necessary library
library(dplyr)

# Assuming your datasets are loaded into data frames called species_count_traitdata and categorical_table
# Read in the datasets (if they are in CSV format)
# species_count_traitdata <- read.csv("path_to_species_count_traitdata.csv")
# categorical_table <- read.csv("path_to_categorical_table.csv")

# Merge the datasets based on AccSpeciesName
merged_data <- merge(species_count_traitdata, Categorical_table, by = "AccSpeciesName", all.x = TRUE)

# The resulting merged_data will contain all rows from species_count_traitdata and only the matching rows from categorical_table
# If you want to keep only the species that are found in both datasets, you can use inner join
matched_data <- merge(species_count_traitdata, Categorical_table, by = "AccSpeciesName")

# View the merged data
head(merged_data)
head(matched_data)


#-------

m_species_count_TRY <- matched_data %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

m_PlantGrowthForm <- matched_data %>%
  group_by(PlantGrowthForm) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

m_LeafType <- matched_data %>%
  group_by(LeafType) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

m_LeafPhenology <- matched_data %>%
  group_by(LeafPhenology) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

m_PhotosyntheticPathway <- matched_data %>%
  group_by(PhotosyntheticPathway) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


#---------

m_PFT_Tree <- matched_data %>%
  filter(PlantGrowthForm %in% c("tree", 
                                "shrub/tree"))

# View the filtered data
print(m_PFT_Tree)


m_PFT_Tree_Broadleaf <- m_PFT_Tree %>%
  filter(LeafType %in% c("broadleaved"))


m_PFT_Tree_Evergreen <- m_PFT_Tree_Broadleaf %>%
  filter(LeafPhenology %in% c("evergreen"))                   # NO. 1+2 BET - Te+Tr


m_PFT_Tree_Evergreen$PFT <- 'BET'



# For BDT ----
m_PFT_Tree_Deciduous <- m_PFT_Tree_Broadleaf %>%
  filter(LeafPhenology %in% c("deciduous"))                   # NO. 3 BDT

m_PFT_Tree_Deciduous$PFT <- 'BDT'



# For NET (needle-leaf trees)----

m_PFT_Needleleaf <- m_PFT_Tree %>%
  filter(LeafType %in% c("scale-shaped/needleleaved", 
                         "scale-shaped/needleleavrd",
                         "needleleaved"))


m_PFT_Needleleaf_Evergreen <- m_PFT_Needleleaf %>%
  filter(LeafPhenology %in% c("evergreen"))                   # NO. 4 NET     

m_PFT_Needleleaf_Evergreen$PFT <- 'NET'



m_PFT_Needleleaf_Deciduous <- m_PFT_Needleleaf %>%
  filter(LeafPhenology %in% c("deciduous"))                   # NO. 5 NDT


m_PFT_Needleleaf_Deciduous$PFT <- 'NDT'



# For shrubs ----
m_PFT_Shrub  <- matched_data %>%
  filter(PlantGrowthForm %in% c("shrub", "herb/shrub"))


m_PFT_Shrub_Evergreen <- m_PFT_Shrub %>%
  filter(LeafPhenology %in% c("evergreen"))                   # NO. 6 ESH

m_PFT_Shrub_Evergreen$PFT <- 'ESH'



m_PFT_Shrub_Deciduous <- m_PFT_Shrub  %>%
  filter(LeafPhenology %in% c("deciduous"))                   # NO. 7 DSH

m_PFT_Shrub_Deciduous$PFT <- 'DSH'



# for grasses ----
m_PFT_Grass <- matched_data %>%
  filter(PlantGrowthForm %in% c("graminoid"))



m_PFT_Grass_C3 <- m_PFT_Grass %>%
  filter(PhotosyntheticPathway %in% c("C3", "C3/CAM"))        # NO. 8 C3


m_PFT_Grass_C3$PFT <- 'C3'


m_PFT_Grass_C4 <- m_PFT_Grass %>%
  filter(PhotosyntheticPathway %in% c("C4", "C4/CAM"))        # NO. 9 C4

m_PFT_Grass_C4$PFT <- 'C4'



#----- # Combine all data frames into one
m_combined_df_PFT <- bind_rows(m_PFT_Tree_Evergreen, m_PFT_Tree_Deciduous, m_PFT_Needleleaf_Evergreen, 
                             m_PFT_Needleleaf_Deciduous, m_PFT_Shrub_Evergreen, m_PFT_Shrub_Deciduous, 
                             m_PFT_Grass_C3, m_PFT_Grass_C4)


# View the combined data frame
print(combined_df_PFT)

