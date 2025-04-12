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


# result summary

table(best_matches$Matched)


table(best_matches$taxonomicStatus)


table(best_matches$New.accepted)


synonyms <- best_matches %>% filter(New.accepted == TRUE)
nrow(synonyms)


unmatched <- best_matches %>% filter(Matched == FALSE)
nrow(unmatched)


summary_table <- best_matches %>%
  group_by(taxonomicStatus, New.accepted) %>%
  summarise(count = n(), .groups = "drop")

print(summary_table)




# optional visualization


library(ggplot2)

# Summary data
summary_data <- data.frame(
  Category = c(
    "Matched - Accepted",
    "Matched - Synonym Replaced",
    "Matched - Unchecked",
    "Unmatched"
  ),
  Count = c(1656, 110, 3, 19)
)

# Add percentages
summary_data <- summary_data %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1),
         Label = paste0(Percentage, "%"))

# Plot with percentage labels
ggplot(summary_data, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "#0072B2", width = 0.7) +
  geom_text(aes(label = Label), vjust = -0.5, size = 6) +
  labs(
    #title = "Taxonomic Harmonization Summary (WorldFlora)",
    y = "Number of species",
    x = "Taxonomic status"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1, size = 22),
    axis.text.y = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
  )




ggsave("Taxonomic_Harmonization_Summary.png", width = 16, height = 10, dpi = 300, bg="white")
