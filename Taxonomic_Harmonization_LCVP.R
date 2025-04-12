# load packages & libraries

library(devtools)
install_github("idiv-biodiversity/LCVP")
install_github("idiv-biodiversity/lcvplants")
library(lcvplants)
library(dplyr)

# load data
Mapped_PFT <- read.csv("Mapped_PFT_data.csv")
species_list <- Mapped_PFT$AccSpeciesName

# sort out invalid entry i.e. species names with weird characters, space or more than 2 words as lcvp doesn't work well with those
valid_binomials <- species_list[
  sapply(strsplit(species_list, " "), function(x) length(x) == 2)
]

results <- lcvp_search(valid_binomials)

# show species that were rejected to be track (this is from the above sorting out of invalid entry)
rejected <- setdiff(species_list, valid_binomials)
writeLines(rejected)

# view summary of the harmonization
lcvp_summary(results)


# combine results with mapped pft data
# Harmonized binomials
harmonized <- cbind(Mapped_PFT[Mapped_PFT$AccSpeciesName %in% valid_binomials, ], results)

# Unmatched species
unmatched <- data.frame(
  AccSpeciesName = rejected,
  Status = "Not harmonized - non-binomial or not found"
)



# Save to file
write.csv(harmonized, "Mapped_PFT_Harmonized_lcvp.csv", row.names = FALSE)
write.csv(unmatched, "Mapped_PFT_LCVP_Unmatched.csv", row.names = FALSE)
