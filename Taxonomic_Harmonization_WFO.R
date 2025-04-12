# Install and Load packages ----
install.packages("WorldFlora", dependencies=TRUE)
library(WorldFlora)
library(dplyr)
library(tidyr)



# Read and clean data file  ----

Mapped_PFT <- read.csv("Mapped_PFT_data.csv")


# Extract species names from the 'Mapped_PFT' dataframe
species_list <- Mapped_PFT$AccSpeciesName

# Download WFO backbone (run this only once)
WFO.download()

# Load WFO backbone into memory
WFO.remember()  # assumes 'classification.csv' is in my working directory

# (optional) Clean names to remove authorship or punctuation
# Only if names include authorship like "(L.) Willd."
prepared <- WFO.prepare(spec.data = species_list)
cleaned_names <- prepared$spec.name  # this will be used for matching

# Match cleaned species names with WFO backbone
matches <- WFO.match(spec.data = cleaned_names, WFO.data = WFO.data)


# Pick the best single match per species
best_matches <- WFO.one(matches)

# Combine harmonized names with original data
selected_columns <- c(
  "spec.name", 
  "scientificName", 
  "taxonomicStatus", 
  "New.accepted", 
  "scientificNameAuthorship", 
  "genus", 
  "family", 
  "taxonID"
)


harmonized_info <- best_matches[, selected_columns]



Mapped_PFT_Harmonized <- cbind(Mapped_PFT, harmonized_info)


# Save to file
write.csv(Mapped_PFT_Harmonized, "Mapped_PFT_Harmonized.csv", row.names = FALSE)


write.csv(best_matches, "best_matches.csv", row.names = FALSE)
