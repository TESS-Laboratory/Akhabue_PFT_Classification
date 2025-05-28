### Step 1 - Load packages ------------------------------------------------



library(europepmc)  # This package is published by https://ropensci.org/, which is really high quality and has a thorough review process.
library(tidyverse)
library(viridis)



# Define your keyword of interest
keyword <- "JULES land surface model"  # Or simply "JULES" for a broader search

# Fetch publication trend data from Europe PMC (1995–2025)
trend_jules <- europepmc::epmc_hits_trend(query = keyword, period = 2000:2025, synonym = FALSE)

# View the full trend data
print(trend_jules)

# Calculate the total number of publications across all years
total_pubs <- sum(trend_jules$query_hits)
cat("Total number of publications referencing '", keyword, "':", total_pubs, "\n")

# Optional: Plot the trend over time
ggplot(trend_jules, aes(x = factor(year), y = query_hits)) +
  geom_col(fill = "#0072B2", width = 0.6) +
  labs(
    title = paste("Publications referencing:", keyword),
    x = "Year",
    y = "Number of Publications"
  ) +
  theme_minimal()
