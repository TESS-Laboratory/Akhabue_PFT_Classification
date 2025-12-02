# Critical Classification Parameters Linking Species to Plant Functional Type in African Ecosystems
---

This was created by Enimhien (efa206@exeter.ac.uk), Andrew Cunliffe, Karina Bett-Williams, Anna Harper, 
Petra Holden, and Tom Powell as part of Enimhien’s Doctoral research within the Oppenheimer Programme in 
African Landscape Systems (OPALS) (https://opals-exeter.org/).
---

The permanent version of this code is available on [![DOI](https://zenodo.org/badge/758078763.svg)](https://doi.org/10.5281/zenodo.16533069)

Repository for species classification into different PFT classes in JULES. 
The species data is from the TRY data base. 

- The JULES_pub_count.R script: This script uses the europepmc R package to search 
for publications related to the JULES land surface model in the Europe PubMed Central 
database. Keywords were used to retrieve relevant literature for background research
and citation tracking.

- Mapped_PFT_Harmonized.csv contains the final output of the plant functional type 
(PFT) mapping and taxonomic harmonization process. Each record includes the mapped 
PFT, and harmonized species names. This is the final output product.

- TRYdata_analysis.R contains the initial steps for accessing and extracting trait
data from the TRY Plant Trait Database using R. The script makes use of methods and
examples adapted from the rtry package, which provides a standardized interface to 
interact with TRY data. This script was used to retrieve plant trait and species 
records for subsequent PFT mapping and analysis. 
  Note: Access to TRY data requires an approved data request and login credentials. 

- Taxonomic_Harmonization_LCVP.R contains an alternative approach for taxonomic 
harmonization using the Leipzig Catalogue of Vascular Plants (LCVP) via the 
lcvplants R package. Although this method was tested during the workflow, it was
not used in the final analysis. The script is retained here as a reference for 
future comparisons or alternative workflows.

- Taxonomic_Harmonization_WFO.R contains the script used for taxonomic harmonization 
of species names using the World Flora Online (WFO) database. The WFO backbone was
downloaded from www.worldfloraonline.org/downloadData on the 12/04/2025.
This method was used in the final workflow to standardize species names, resolve synonyms,
and ensure consistency across plant trait records. The harmonized output served as the basis 
for the final dataset in Mapped_PFT_Harmonized.csv.

- Workflow_for_PFT_classification.R contains the full processing pipeline for classifying
plant species into Plant Functional Types (PFTs). This script brings together the 
trait data extraction, PFT assignment, and taxonomic harmonization (based on the 
World Flora Online approach), and generates the final cleaned dataset used for 
analysis (Mapped_PFT_Harmonized.csv).

- DATA_column_descriptions.csv provides definitions and descriptions for each column 
in the dataset. This metadata file serves as a reference to understand the contents 
and structure of the data.

- Traits_observed_from_TRY_Database.csv provides the list of trait observation requested
from the TRY database with their acoompanying trait ID as in TRY. 
