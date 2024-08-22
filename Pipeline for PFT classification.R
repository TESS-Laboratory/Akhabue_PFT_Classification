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

# ✓ Assign each specie a PFT class based on the information from the lookup table
# sort the assigned species according to the species from your work data - as these are the species you will be working with
# merge the new df - the pft classification data - to match with your working data 









# Code--------------------------------------------

# Load packages ----
library(tidyverse)
library(viridis)  # For color scales
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# Read and clean data file  ----

Trait_species <- read.csv("trait_africa.csv")


# Other data from csv - This data is the list of species in the trait data not found in the categorical table. I had to export and carry out the classification exercise outside R
PFT_CLASS <- read.csv("PFT_CLASS.csv")


# load global data - The global trait data for QC purpose
Global_traitdata <- read.csv("trait_data.csv")


# Look up table
Categorical_table <- read.csv("TRY_Categorical_Traits_Lookup_Table.csv")

# Keep only the columns we need for the categorical look up table
vars <- c("AccSpeciesID", "AccSpeciesName", "Genus", "SpeciesEpithet", "Family", "PlantGrowthForm", "LeafType",
          "LeafPhenology", "PhotosyntheticPathway")

Categorical_table<- Categorical_table %>% dplyr::select(one_of(vars))



# Remove rows where AccSpeciesID is 25135 or 62840 - these are duplicates. There are other duplicates in the categorical table, but only these 2 species occur in the trait data, so I have focused on them only
Categorical_table <- Categorical_table %>%
  filter(!(AccSpeciesID %in% c(25135, 62840)))



# Sort the unique species from my working data ----
# Count the occurrences of each species from my observation
species_count_traitdata <- Trait_species %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# for global data
species_count_traitdataglobal <- Global_traitdata %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# for categorical table
species_count_TRY <- Categorical_table %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# Merge the datasets based on AccSpeciesName
merged_data <- merge(species_count_traitdata, Categorical_table, by = "AccSpeciesName", all.x = TRUE)

# The resulting merged_data will contain all rows from species_count_traitdata and only the matching rows from categorical_table
# to keep only the species that are found in both datasets, use inner join

matched_data <- merge(species_count_traitdata, Categorical_table, by = "AccSpeciesName")




# Sort unique values for the other variables in the categorical table eg leaf type, plant growth form, leaf phenology, photosynthetic pathway
# this is to know the different variables and their individual count
# this is also important for QC to help indicate for duplicates and solve that problem
# After the sorting was done, it was easy to identify the species with missing information and what information was needed


# For plant growth form ----
species_to_updateTrees <- c("Vitellaria paradoxa", "Pouteria alnifolia", "Scorodophloeus zenkeri", "Triplochiton scleroxylon", "Gymnosporia buxifolia", 
                            "Nesogordonia papaverifera", "Cola gigantea", "Blighia sapida", "Chrysophyllum boivinianum", "Cryptocarya thouvenotii",
                            "Tridesmostemon omphalocarpoides", "Blighia welwitschii", "Tessmannia africana", "Ocotea racemosa", "Strombosia grandifolia", 
                            "Trichilia monadelpha", "Detarium senegalense", "Quassia undulata", "Vitex grandifolia", "Sterculia rhinopetala", "Faidherbia albida", 
                            "Dalbergia baronii", "Hymenostegia afzelii", "Beilschmiedia velutina", "Ocotea auriculiformis", "Sterculia oblonga", "Strombosiopsis tetrandra",
                            "Chrysophyllum africanum", "Trichocladus ellipticus", "Strombosia pustulata", "Terminalia kilimandscharica", "Turraeanthus africanus", 
                            "Buddleja saligna", "Ongokea gore", "Pterygota macrocarpa", "Synsepalum brevipes", "Virgilia oroboides", "Mansonia altissima", "Vitex welwitschii",
                            "Chrysophyllum lacourtianum", "Nuxia capitata", "Prioria oxyphylla", "Schotia brachypetala", "Strychnos spinosa", "Pentaclethra macrophylla", 
                            "Monanthotaxis caffra", "Pterocarpus soyauxii", "Elaeodendron transvaalense", "Tabernaemontana crassa", "Inhambanella henriquezii", 
                            "Paraserianthes lophantha", "Vepris trichocarpa", "Cornus volkensii", "Gilletiodendron mildbraedii", "Pouteria adolfi-friedericii",
                            "Labramia louvelii", "Saba comorensis", "Napoleonaea imperialis", "Parkia bicolor", "Landolphia owariensis", "Paramacrolobium coeruleum", 
                            "Julbernardia paniculata", "Millettia drastica", "Pericopsis angolensis", "Philenoptera violacea", "Securidaca longipedunculata",
                            "Vitex chrysocarpa", "Brachystegia floribunda", "Erythrina senegalensis", "Philenoptera nelsii", "Ptychopetalum petiolatum", 
                            "Tiliacora funifera", "Treculia obovoidea", "Alantsilodendron pilosum", "Boswellia dalzielii", "Commiphora glandulosa", 
                            "Faurea saligna", "Gardenia ternifolia", "Haematostaphis barteri", "Maerua crassifolia", "Pouteria altissima", "Tetraberlinia bifoliolata", 
                            "Vepris lanceolata", "Chasmanthera dependens", "Coffea liberica", "Colpoon compressum", "Didelotia africana", "Elaeodendron croceum", 
                            "Englerophytum oblanceolatum", "Grewia arborea", "Gymnosporia mossambicensis", "Isoberlinia tomentosa", "Lettowianthus stellatus", "Ludia mauritiana", 
                            "Maerua edulis", "Maerua triphylla", "Manilkara fouilloyana", "Manilkara sulcata", "Medusandra richardsiana", "Microberlinia bisulcata", 
                            "Monanthotaxis fornicata", "Monanthotaxis parvifolia", "Monodora myristica", "Nauclea latifolia", "Nesogordonia holtzii", "Ocotea gabonensis", 
                            "Octoknema borealis", "Omphalocarpum elatum", "Oncoba routledgei", "Ophiobotrys zenkeri", "Philenoptera laxiflora", "Pouteria pierrei", "Premna maxima", 
                            "Pterocarpus santalinoides", "Rothmannia engleriana", "Rothmannia manganjae", "Saba senegalensis", "Scottellia kamerunensis", "Senna singueana", 
                            "Sterculia dawei", "Strephonema pseudocola", "Syzygium parvifolium", "Syzygium sclerophyllum", "Tetrapleura tetraptera", "Xylopia arenaria", 
                            "Adenia kigogoensis", "Adenia litoralis", "Anthyllis henoniana", "Dalbergia melanixilum", "Diospyros elliotii", "Julbernardia pellegriniana", "Keetia cornelia",
                            "Milicia regia", "Millettia griffoniana", "Millettia laurentii", "Millettia usaramensis", "Newbouldia laevis", "Newtonia paucijuga", "Oxystigma oxyphyllum", 
                            "Senna spectabilis", "Sindoropsis letestui", "Sterculia setigera", "Thespesia garckeana", "Vitex fischeri", "Vitex keniensis", 
                            "Vitex rivularis", "Xanthocercis zambesiaca", "Zanthoxylum zanthoxyloides", "Pausinystalia pynaertii", "Lonchocarpus laxiflorus")



matched_data <- matched_data %>%
  mutate(PlantGrowthForm = ifelse(AccSpeciesName %in% species_to_updateTrees & PlantGrowthForm == "", 
                                  "tree", PlantGrowthForm))





species_to_updateShrubs <- c("Lycium shawii", "Haloxylon scoparium", "Helianthemum lippii", "Gymnosporia senegalensis", "Koelpinia linearis", "Protea cynaroides", 
                             "Notoceras bicorne", "Protea punctata", "Protea mundii", "Protea longifolia", "Melhania velutina", "Protea eximia", "Protea coronata", 
                             "Rubus steudneri", "Helichrysum newii", "Astragalus armatus", "Conyza pyrrhopappa", "Senecio maranguensis", "Crassula pellucida", 
                             "Culcasia falcifolia", "Protea magnifica", "Protea nana", "Helichrysum odoratissimum", "Erigeron bonariensis", "Protea acuminata",
                             "Ocimum obovatum", "Protea lorifolia", "Plectranthus lasianthus", "Melianthus major", "Hyptis suaveolens", "Pancovia harmsiana", 
                             "Pericopsis laxiflora", "Pancovia laurentii", "Plectranthus autranii", "Rourea thomsonii", "Laurophyllus capensis", "Protea aurea",
                             "Leucadendron salicifolium", "Polygala myrtifolia", "Syncarpha argyropsis", "Synsepalum subcordatum", "Solanum linnaeanum", 
                             "Leucadendron spissifolium", "Pringlea antiscorbutica", "Polyscias ornifolia", "Loeseneriella africana", "Pavetta abyssinica", 
                             "Morella serrata", "Athanasia trifurcata", "Passerina paleacea", "Paullinia pinnata", "Combretum aculeatum", "Opilia amentacea", 
                             "Erica hispidula", "Griffonia simplicifolia", "Alafia barteri", "Dalbergia obovata", "Strophanthus sarmentosus", "Urera hypselodendron", 
                             "Jasminum abyssinicum", "Vitex congolensis", "Cadaba farinosa", "Chassalia cristata", "Clematis simensis", "Elachyptera parvifolia", 
                             "Flemingia macrophylla", "Massularia acuminata", "Neuropeltis acuminata", "Passiflora edulis", "Protea madiensis", "Sericostachys scandens",
                             "Stephania abyssinica", "Combretum nioroense", "Tetracera alnifolia", "Vepris nobilis", "Adenia lobata", "Adenia metamorpha", "Artabotrys modestus",
                             "Cadaba glandulosa", "Clerodendrum capitatum", "Clerodendrum fuscum", "Clerodendrum splendens", "Combretum bipindense", "Combretum tomentosum", 
                             "Cryptolepis sanguinolenta", "Elytropappus rhinocerotis", "Flabellaria paniculata", "Gardenia sokotensis", "Grewia laevigata", "Grewia lasiodiscus",
                             "Grewia stuhlmannii", "Henophyton deserti", "Lagenaria abyssinica", "Leucadendron corymbosum", "Lippia multiflora", "Lobularia libyca", 
                             "Lycium schweinfurthii", "Maerua edulis", "Maerua triphylla", "Memecylon fragrans", "Memecylon lateriflorum", "Microglossa densiflora", 
                             "Mimetes fimbriifolius", "Mitragyna rubrostipulata", "Mussaenda erythrophylla", "Ochna thomasiana", "Olax subscorpioides", "Oxyanthus formosus", 
                             "Oxyanthus zanguebaricus", "Pleiocarpa bicarpellata", "Polysphaeria parvifolia", "Premna chrysoclada", "Premna oligotricha", "Protea caffra", 
                             "Psilotrichum africanum", "Psychotria alsophila", "Psychotria crassipetala", "Psychotria pseudoplatyphylla", "Psydrax faulknerae", "Psydrax polhillii", 
                             "Pyrostria phyllanthoidea", "Rapanea rhododendroides", "Ritchiea capparoides", "Rourea coccinea", "Rutidea olenotricha", "Rutidea orientalis", 
                             "Sageretia thea", "Scaphopetalum blackii", "Secamone afzelii", "Solanum terminale", "Stictocardia beraviensis", "Strombosia zenkeri", 
                             "Strychnos panganensis", "Tinospora caffra", "Triclisia patens", "Uvaria schweinfurthii", "Uvariopsis congolana", "Vernonia zanzibarensis", "Vitex doniana", 
                             "Commiphora saxicola", "Craterispermum laurinum", "Entada gigas", "Entada wahlbergii", "Haloxylon salicornicum", "Jasminum fluminense", 
                             "Jasminum grandiflorum", "Leptadenia arborea", "Philenoptera laxiflora", "Ononis serrata", "Otholobium fruticans", "Pterocarpus antunesii", 
                             "Senna italica", "Senna petersiana", "Senna podocarpa", "Strophanthus hispidus", "Vepris uguenensis", "Vitex ferruginea", "Halogeton alopecuroides", 
                             "Begonia meyeri-johannis", "Scaphopetalum thonneri", "Adenia cissampeloides", "Baissea axillaris", "Hugonia castaneifolia", 
                             "Mezoneuron benthamianum", "Microglossa pyrifolia", "Adenia mcdadiana", "Alafia microstylis", "Silene succulenta", "Cissus verticillata")



matched_data <- matched_data %>%
  mutate(PlantGrowthForm = ifelse(AccSpeciesName %in% species_to_updateShrubs & PlantGrowthForm == "", 
                                  "shrub", PlantGrowthForm))




PlantGrowthForm <- matched_data %>%
  group_by(PlantGrowthForm) %>%
  summarise(count = n()) %>%
  arrange(desc(count))





# For Leaf type ----
matched_data$LeafType[which(matched_data$LeafType == "")] <- "broadleaved"

LeafType <- matched_data %>%
  group_by(LeafType) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



# For leaf phenology ----
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
  mutate(LeafPhenology = ifelse(AccSpeciesName == "Pouteria pierrei" & LeafPhenology == "evergreen", 
                                "deciduous", LeafPhenology))



matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName == "Strophanthus hispidus" & LeafPhenology == "evergreen", 
                                "deciduous", LeafPhenology))




matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName == "Strophanthus sarmentosus" & LeafPhenology == "evergreen", 
                                "deciduous", LeafPhenology))





species_to_update3 <- c("Markhamia obtusifolia", "Monodora angolensis", "Terminalia avicennioides", 
                        "Terminalia brachystemma", "Terminalia mollis", "Terminalia prunioides", 
                        "Vepris zambesiaca", "Maerua angolensis", "Maerua prittwitzii", "Maesopsis eminii",
                        "Monodora junodii", "Nectaropetalum kaessneri", "Newtonia buchananii",
                        "Khaya grandifoliola", "Ozoroa insignis", "Aidia genipiflora",
                        "Khaya anthotheca", "Ficus sycomorus", "Combretum mossambicense", "Chaetachme aristata", 
                        "Cola griseiflora", "Albizia ferruginea", "Ficus lutea", "Brachylaena ramiflora", "Hagenia abyssinica",
                        "Bauhinia thonningii", "Ricinodendron heudelotii", "Lannea welwitschii", "Dalbergia armata", 
                        "Brachystegia boehmii", "Lannea schweinfurthii", "Cordia sinensis", "Combretum fruticosum",
                        "Bridelia scleroneura", "Isoberlinia angolensis", "Albizia harveyi", "Cleistochlamys kirkii",
                        "Commiphora neglecta", "Diospyros alboflavescens", "Grewia mollis", 
                        "Klaineanthus gaboniae", "Berchemia discolor", "Cecropia concolor", "Dialium excelsum", "Guibourtia demeusei",
                        "Mitragyna inermis", "Ozoroa obovata", "Pseudocedrela kotschyi", "Psorospermum febrifugum", 
                        "Baillonella toxisperma", "Berlinia grandiflora", "Commiphora mollis", "Croton sylvaticus",
                        "Entandrophragma cylindricum", "Glyphaea brevis", "Grewia tembensis", "Lannea microcarpa", "Macaranga capensis",
                        "Phyllanthus pinnatus", "Steganotaenia araliacea", "Uvaria welwitschii", "Brachystegia utilis", "Capparis fascicularis",
                        "Combretum erythrophyllum", "Commiphora engleri", "Ficus stuhlmannii", "Lannea discolor", "Oncoba spinosa", "Strychnos cocculoides",
                        "Terminalia stenostachya", "Thespesia acutiloba", "Bauhinia tomentosa", "Berlinia bracteosa", "Bombax rhodognaphalon",
                        "Carpodiptera africana", "Combretum elaeagnoides", "Commiphora campestris", "Commiphora mossambicensis", "Cussonia arborea", 
                        "Desplatsia dewevrei", "Dialium dinklagei", "Ehretia bakeri", "Entandrophragma utile", "Kirkia acuminata", "Phyllanthus inflatus", 
                        "Sterculia appendiculata", "Vangueria madagascariensis", "Afzelia bella", "Albizia amara", "Albizia anthelmintica", "Albizia chevalieri", 
                        "Albizia versicolor", "Albizia zimmermannii", "Amphimas ferrugineus", "Balanites pedicellaris", "Boswellia neglecta", 
                        "Brachystegia bakeriana", "Brachystegia microphylla", "Brachystegia wangermeeana", "Commiphora confusa", "Commiphora pteleifolia", 
                        "Commiphora ugogensis", "Commiphora unilobata", "Commiphora zanzibarica", "Dalbergia nitidula", "Daniellia klainei",
                        "Cremaspora triflora", "Englerodendron usambarense", "Entada abyssinica", "Entandrophragma caudatum", "Kirkia tenuifolia", "Lannea humilis",
                        "Lannea schimperi", "Lannea triphylla", "Prosopis chilensis", "Koelpinia linearis", "Malvastrum coromandelianum", "Solanum dulcamara",
                        "Ocimum americanum", "Leptadenia pyrotechnica", "Grewia caffra", "Cissus quadrangularis", "Capparis tomentosa", "Rhoicissus tridentata",
                        "Rubus apetalus", "Bauhinia rufescens", "Maerua endlichii", "Bauhinia reticulata", "Canthium glaucum", "Combretum exalatum", 
                        "Dichapetalum crassifolium", "Echinops spinosissimus", "Grewia tenax", "Calligonum polygonoides", "Combretum celastroides", 
                        "Lannea alata", "Sorindeia juglandifolia", "Tephrosia polystachya", "Zornia glochidiata", "Plantago ovata", "Achillea leptophylla", 
                        "Galinsoga parviflora", "Oxalis corniculata", "Acanthospermum hispidum", "Launaea cornuta", "Argemone mexicana", 
                        "Zaluzianskya villosa", "Chenopodium album", "Nicandra physalodes", "Acaena magellanica", "Lablab purpureus", 
                        "Peganum harmala", "Adenia volkensii", "Cucumis melo", "Reichardia tingitana", "Vigna unguiculata", "Watsonia borbonica", "Pouteria alnifolia", 
                        "Triplochiton scleroxylon", "Rubus steudneri", "Nesogordonia papaverifera", "Ocimum obovatum", "Sterculia rhinopetala", 
                        "Dalbergia baronii", "Pancovia harmsiana", "Sterculia oblonga", "Pericopsis laxiflora", "Pancovia laurentii", "Terminalia kilimandscharica",
                        "Pterygota macrocarpa", "Vitex welwitschii", "Loeseneriella africana", "Strychnos spinosa", "Gilletiodendron mildbraedii", "Paullinia pinnata", 
                        "Combretum aculeatum", "Parkia bicolor", "Dalbergia obovata", "Millettia drastica", "Pericopsis angolensis", "Philenoptera violacea", 
                        "Securidaca longipedunculata", "Vitex chrysocarpa", "Clematis simensis", "Erythrina senegalensis", "Flemingia macrophylla", "Philenoptera nelsii", 
                        "Stephania abyssinica", "Alantsilodendron pilosum","Boswellia dalzielii", "Combretum nioroense", "Commiphora glandulosa", "Haematostaphis barteri", 
                        "Adenia lobata", "Cadaba glandulosa", "Chasmanthera dependens", "Combretum bipindense", "Combretum tomentosum", "Flabellaria paniculata", 
                        "Grewia arborea", "Grewia lasiodiscus", "Isoberlinia tomentosa", "Microberlinia bisulcata", "Philenoptera laxiflora", "Ritchiea capparoides", 
                        "Senna singueana", "Solanum terminale", "Sterculia dawei", "Stictocardia beraviensis", "Tinospora caffra", 
                        "Vitex doniana", "Dalbergia melanixilum", "Entada wahlbergii", "Jasminum grandiflorum", "Julbernardia pellegriniana", "Philenoptera laxiflora",
                        "Millettia griffoniana", "Millettia laurentii", "Millettia usaramensis", "Newtonia paucijuga", "Pterocarpus antunesii", "Senna italica", 
                        "Senna petersiana", "Senna podocarpa", "Senna spectabilis", "Sindoropsis letestui", "Sterculia setigera", "Vitex ferruginea", "Vitex fischeri", 
                        "Vitex keniensis", "Halogeton alopecuroides", "Stephania cyanantha", "Mezoneuron benthamianum") 



matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName %in% species_to_update3 & LeafPhenology == "", 
                                "deciduous", LeafPhenology))






species_to_update4 <- c("Terminalia sambesiaca", "Terminalia sambesiaca", "Mammea bongo", "Manilkara sansibarensis", 
                        "Maranthes glabra", "Mareya micrantha", "Melanophylla crenata", "Mimusops obtusifolia", 
                        "Monodora grandidieri", "Morinda lucida", "Aphloia theiformis", "Trilepisium madagascariense", 
                        "Bridelia cathartica", "Gilbertiodendron dewevrei", "Tabernaemontana stapfiana", "Hypericum revolutum",
                        "Rhamnus prinoides", "Apodocephala pauciflora", "Olinia rochetiana", "Streblus dimepate", "Uapaca louvelii",
                        "Weinmannia rutenbergii", "Halleria lucida", "Anonidium mannii", "Cavacoa quintasii", "Entada louvelii", 
                        "Leptonychia usambarensis", "Trichilia prieuriana", "Afrocarpus falcatus", "Weinmannia bojeriana",
                        "Isolona thonneri", "Senna siamea", "Bridelia tulasneana", "Burchellia bubalina", "Melanophylla crenata",
                        "Platylophus trifoliatus", "Gonioma kamassi", "Erythrophleum suaveolens", "Croton megalocarpus",
                        "Landolphia buchananii", "Albizia tomentosa", "Casearia battiscombei", "Cynometra hankei",
                        "Oncinotis tenuiloba", "Salacia leptoclada", "Diospyros boala", "Brachylaena discolor", "Canthium oligocarpum",
                        "Dalbergia lactea", "Trichilia emetica", "Aulacocalyx jasminiflora", "Baikiaea insignis", "Polyscias fulva",
                        "Balanites wilsoniana", "Calotropis procera", "Croton pseudopulchellus", "Discoglypremna caloneura", 
                        "Kigelia africana", "Manilkara mochisia", "Acalypha glabrata", "Aidia micrantha", "Allanblackia stuhlmannii",
                        "Antrocaryon nannanii", "Boscia coriacea", "Brachylaena huillensis", "Chrysophyllum albidum", 
                        "Cola chlamydantha", "Cola congolana", "Diospyros bipindensis", "Diospyros cinnabarina", "Dombeya kirkii", 
                        "Drypetes angustifolia", "Heisteria parvifolia", "Hexalobus crispiflorus", "Ozoroa paniculosa", 
                        "Pleiocarpa pycnantha", "Rinorea ilicifolia", "Polyscias albersiana", "Rinorea welwitschii", "Anthonotha fragrans", 
                        "Asteranthe asterias", "Dacryodes edulis", "Erythrophleum ivorense", "Julbernardia seretii", "Uapaca kirkiana",
                        "Afzelia bipindensis", "Allophylus africanus", "Angylocalyx pynaertii", "Anthocleista grandiflora", "Antidesma membranaceum",
                        "Baphia wollastonii", "Bourreria petiolaris", "Catunaregam nilotica", "Chassalia umbraticola", "Chrysophyllum perpulchrum", 
                        "Cola acuminata", "Cola lepidota", "Cola minor", "Cola pachycarpa", "Cola rostrata", "Commiphora schimperi", 
                        "Copaifera mildbraedii", "Coptosperma nigrescens", "Coptosperma supra-axillare", "Cordia dentata", "Cordia monoica",
                        "Cynometra alexandri", "Dacryodes klaineana", "Dasylepis integra", "Diospyros barteri", "Diospyros canaliculata",
                        "Diospyros suaveolens", "Drypetes molunduana", "Drypetes paxii", "Drypetes reticulata", "Drypetes usambarica", "Euphorbia cuneata",
                        "Ficus artocarpoides", "Ficus craterostoma", "Ficus oreodryadum", "Ficus tremula", "Funtumia africana", "Galpinia transvaalica", 
                        "Grewia similis", "Haplocoelum inoploeum", "Harrisonia abyssinica", "Hypodaphnis zenkeri", "Irvingia robur", "Keetia zanzibarica", 
                        "Lasiodiscus mannii", "Macaranga barteri", "Macaranga conglomerata", "Pentadesma grandifolia", "Pleurostylia africana", "Poga oleosa", 
                        "Psydrax parviflora", "Pyrostria bibracteata", "Rhigozum zambesiacum", "Rytigynia uhligii", "Tabernaemontana pachysiphon", 
                        "Thomandersia laurifolia", "Turraea holstii", "Uvaria caffra", "Afrostyrax lepidophyllus", "Allophylus pervillei", "Aloe littoralis", 
                        "Baikiaea robynsii", "Caesalpinia decapetala", "Chytranthus carneus", "Citropsis daweana", "Craibia zimmermannii", 
                        "Cylicodiscus gabunensis", "Cynometra webberi", "Dialium orientale", "Eriocoelum microspermum", "Filicium decipiens", "Flacourtia indica", 
                        "Gossweilerodendron balsamiferum", "Hylodendron gabunense", "Hymenaea verrucosa", "Hymenostegia pellegrinii", "Isoberlinia scheffleri", 
                        "Lecaniodiscus cupanioides", "Trichilia capitata", "Trichilia dregeana", "Trichilia welwitschii", "Uapaca nitida", "Uvariopsis congensis", 
                        "Helianthemum lippii", "Gymnosporia senegalensis", "Pseudarthria hookeri", "Aspilia mossambicensis", "Stylosanthes fruticosa", 
                        "Piper capense", "Pauridiantha paucinervis", "Dracaena fragrans", "Carissa spinarum", "Rhoicissus tomentosa", "Capparis sepiaria", 
                        "Lantana camara", "Cassinopsis ilicifolia", "Cliffortia ruscifolia", "Penaea mucronata", "Protea lepidocarpodendron", "Combretum paniculatum",
                        "Phytolacca dodecandra", "Cyphostemma adenocaule", "Pisonia aculeata", "Rhaphiostylis beninensis", "Gardenia erubescens", "Apodostigma pallens", 
                        "Capparis erythrocarpos", "Cissus producta", "Dovyalis macrocalyx", "Keetia gueinzii", "Salacia congolensis", "Salacia elegans",
                        "Salacia erecta", "Acalypha bipartita", "Acalypha fruticosa", "Anisotes sessiliflorus", "Artabotrys monteiroae", "Azima tetracantha", 
                        "Carissa tetramera", "Carpolobia goetzei", "Cissus populnea", "Coptosperma graveolens", "Deinbollia oblongifolia", "Dovyalis caffra",
                        "Erythrococca bongensis", "Ficus asperifolia", "Gossypioides kirkii", "Jasminum dichotomum", "Keetia venosa", "Landolphia kirkii", 
                        "Lindackeria bukobensis", "Mussaenda arcuata", "Pseudospondias microcarpa", "Ruschia karrooica", "Salacia madagascariensis",
                        "Strychnos lucens", "Thecacoris spathulifolia", "Uvaria acuminata", "Vernonia brachycalyx", "Whitfieldia elongata", "Acanthosicyos horridus", 
                        "Allocassine laurifolia", "Cajanus cajan", "Commiphora rostrata", "Enarganthe octonaria", "Ephedra alata", "Salacia chlorantha", 
                        "Xylotheca kraussiana", "Erythrococca polyandra", "Grewia forbesii", "Grewia holstii", "Anthospermum herbaceum", "Eruca vesicaria", 
                        "Plantago albicans", "Oxalis obliquifolia", "Bidens pilosa", "Senna tora", "Stellaria media", "Pilea usambarensis", "Kohautia caespitosa",
                        "Phyllanthus odontadenius", "Cleome gynandra", "Thunbergia alata", "Disperis dicerochila", "Pelargonium capitatum",
                        "Pelargonium rapaceum", "Pelargonium carnosum", "Pelargonium triste", "Pelargonium myrrhifolium", "Droguetia iners",
                        "Pelargonium betulinum", "Solanum nigrum", "Pelargonium crithmifolium", "Pelargonium echinatum", "Pelargonium citronellum", 
                        "Pelargonium grandicalcaratum", "Pelargonium scabroide", "Pelargonium senecioides", "Anthospermum aethiopicum", 
                        "Pelargonium magenteum", "Pelargonium alternans", "Zantedeschia aethiopica", "Thamnochortus cinereus", 
                        "Hypoestes aristata", "Pelargonium candicans", "Pelargonium reflexipetalum", "Pelargonium articulatum",  "Pelargonium asarifolium", 
                        "Pelargonium chamaedryfolium", "Pelargonium cordifolium", "Pelargonium karooicum", "Pelargonium longifolium", "Pelargonium oblongatum", 
                        "Pelargonium papilionaceum", "Pelargonium scabrum", "Pelargonium sericifolium", "Pelargonium ternatum", "Pelargonium ternifolium", 
                        "Montia fontana", "Pelargonium alchemilloides", "Pelargonium anethifolium", "Pelargonium cucullatum", "Pelargonium grossularioides", 
                        "Pelargonium lobatum", "Pelargonium pinnatum", "Pelargonium zonale", "Pelargonium columbinum", "Pelargonium pillansii", 
                        "Pelargonium radulifolium", "Pelargonium stipulaceum", "Chamaecrista absus", "Pelargonium antidysentericum", 
                        "Pelargonium dasyphyllum", "Pelargonium exstipulatum", "Pelargonium praemorsum", "Pelargonium incrassatum", 
                        "Asparagus officinalis", "Basella alba", "Callitriche antarctica", "Ipomoea coptica", "Ipomoea hochstetteri", "Pancratium maritimum", 
                        "Plumbago zeylanica", "Syncarpha vestita", "Tribulus terrestris", "Zehneria minutiflora", "Aristea capitata", "Protea cynaroides", 
                        "Notoceras bicorne", "Protea punctata", "Protea mundii", "Protea longifolia", "Scorodophloeus zenkeri", "Melhania velutina", 
                        "Protea eximia", "Protea coronata", "Helichrysum newii", "Senecio maranguensis", "Culcasia falcifolia", 
                        "Protea magnifica", "Protea nana", "Gymnosporia buxifolia", "Helichrysum odoratissimum", "Erigeron bonariensis", "Cola gigantea",
                        "Protea acuminata", "Protea lorifolia", "Chrysophyllum boivinianum", "Plectranthus lasianthus", "Tridesmostemon omphalocarpoides", 
                        "Melianthus major", "Blighia welwitschii", "Tessmannia africana", "Strombosia grandifolia", "Trichilia monadelpha", 
                        "Detarium senegalense", "Quassia undulata", "Vitex grandifolia", "Hymenostegia afzelii", "Strombosiopsis tetrandra", "Chrysophyllum africanum",
                        "Plectranthus autranii", "Rourea thomsonii", "Laurophyllus capensis", "Protea aurea", "Trichocladus ellipticus", "Strombosia pustulata", 
                        "Leucadendron salicifolium", "Polygala myrtifolia", "Synsepalum subcordatum", "Turraeanthus africanus", "Solanum linnaeanum", 
                        "Buddleja saligna", "Ongokea gore", "Synsepalum brevipes", "Virgilia oroboides", "Mansonia altissima", "Pringlea antiscorbutica", 
                        "Chrysophyllum lacourtianum", "Nuxia capitata", "Polyscias ornifolia", "Prioria oxyphylla", "Schotia brachypetala", "Pavetta abyssinica", 
                        "Morella serrata", "Monanthotaxis caffra", "Pterocarpus soyauxii", "Inhambanella henriquezii", "Paraserianthes lophantha", "Athanasia trifurcata", 
                        "Vepris trichocarpa", "Cornus volkensii", "Passerina paleacea", "Labramia louvelii", "Opilia amentacea", "Erica hispidula",
                        "Griffonia simplicifolia", "Cissus verticillata", "Paramacrolobium coeruleum", "Urera hypselodendron", "Jasminum abyssinicum", 
                        "Julbernardia paniculata", "Brachystegia floribunda", "Cadaba farinosa", "Chassalia cristata", "Elachyptera parvifolia", "Massularia acuminata", 
                        "Neuropeltis acuminata", "Ptychopetalum petiolatum", "Sericostachys scandens", "Tiliacora funifera", "Maerua crassifolia", 
                        "Tetraberlinia bifoliolata", "Tetracera alnifolia", "Vepris lanceolata", "Vepris nobilis", "Adenia metamorpha", "Artabotrys modestus", 
                        "Cissus petiolata", "Clerodendrum capitatum", "Clerodendrum fuscum", "Clerodendrum splendens", "Coffea liberica", "Colpoon compressum", 
                        "Cryptolepis sanguinolenta", "Didelotia africana", "Elaeodendron croceum", "Elytropappus rhinocerotis", "Englerophytum oblanceolatum", 
                        "Gardenia sokotensis", "Grewia laevigata", "Gymnosporia mossambicensis", "Leucadendron corymbosum", "Lippia multiflora", "Manilkara sulcata", 
                        "Medusandra richardsiana", "Microglossa densiflora", "Mimetes fimbriifolius", "Mitragyna rubrostipulata", "Monanthotaxis fornicata", "Mussaenda erythrophylla", 
                        "Nauclea latifolia", "Nesogordonia holtzii", "Octoknema borealis", "Omphalocarpum elatum", "Oncoba routledgei", "Ophiobotrys zenkeri", "Oxyanthus formosus", 
                        "Oxyanthus zanguebaricus", "Polysphaeria parvifolia", "Premna maxima", "Premna oligotricha", "Protea caffra", "Psilotrichum africanum", "Psychotria alsophila", 
                        "Psychotria crassipetala", "Pterocarpus santalinoides", "Pyrostria phyllanthoidea", "Rothmannia engleriana", "Rothmannia manganjae", "Rourea coccinea", 
                        "Sageretia thea", "Scottellia kamerunensis", "Smilax aspera", "Strephonema pseudocola", "Strychnos panganensis", "Triclisia patens", 
                        "Uvariopsis congolana", "Vernonia zanzibarensis", "Adenia kigogoensis", "Adenia litoralis", "Anthyllis henoniana", "Commiphora saxicola", 
                        "Craterispermum laurinum", "Diospyros elliotii", "Entada gigas", "Jasminum fluminense", "Keetia cornelia", "Newbouldia laevis", "Ononis serrata", 
                        "Otholobium fruticans", "Oxystigma oxyphyllum", "Thespesia garckeana", "Vepris uguenensis", "Vitex rivularis", "Zanthoxylum zanthoxyloides", 
                        "Lycium shawii", "Adenia cissampeloides", "Cissampelos owariensis", "Pausinystalia pynaertii", "Adenia mcdadiana")



matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName %in% species_to_update4 & LeafPhenology == "", 
                                "evergreen", LeafPhenology))





LeafPhenology <- matched_data %>%
  group_by(LeafPhenology) %>%
  summarise(count = n()) %>%
  arrange(desc(count))







# For Photosynthetic pathway C3 and C4 ----
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





# Combine both dataset - combining the matched dataset with complete information and the other data that was sorted outside of R to get the complete species data collected for this study
matched_data <- bind_rows(matched_data, PFT_CLASS)




# group species into different PFT classes
# At this stage I will combine the BET-Tr (tropical broadleaf evergreen trees), and
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


PFT_Tree_Evergreen$PFT <- 'BET-Tr'



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
  filter(LeafPhenology %in% c("deciduous"))                   # NO. 5 NDT So far, no indication that any species in my dataset falls under this class


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




# Perform a left join to add PFT information to Trait_species
Trait_species_with_PFT <- Trait_species %>%
  left_join(combined_df_PFT %>%
              select(AccSpeciesName, PFT), 
            by = "AccSpeciesName")



# omit NA data. these are species that could not be classified due to limited information and available resources

Trait_species_with_PFT <- Trait_species_with_PFT[!is.na(Trait_species_with_PFT$PFT), ]

# remove columns not useful 
Trait_species_with_PFT <- subset(Trait_species_with_PFT, select = -c(geometry, geo))


# export data 
write.csv(Trait_species_with_PFT, "PFT_trait_data.csv", row.names = FALSE)


















# Visualization of data to ascertain the value of PFT classes in my data and what steps can be taken further ----

# 1. PFT Distribution Map----
#This map will show the geographic distribution of different Plant Functional Types (PFTs).

# Get world map data and filter for African countries
world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- sf::st_make_valid(world[world$continent == "Africa", ])
Trait_species_with_PFT_sf <- Trait_species_with_PFT %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_filter(africa)


(ggplot() +
    geom_sf(data = africa, fill = NA, color = "black") +
    geom_sf(data = Trait_species_with_PFT_sf, aes(color = PFT)) +
    coord_sf() +
    theme_void() +
    labs(x = "Longitude", y = "Latitude", color = "PFT") +
    guides(color = guide_legend(title = "PFT")) +
    ggtitle("Geographic distribution of PFTs in Africa")) 



# 2. PFT Density Plot----
# This plot will visualize the density of PFTs in different regions. You could use a hexbin plot or kernel density estimation.

pft_counts <- Trait_species_with_PFT %>%
  count(PFT) %>%
  rename(count = n)


# Prepare custom legend labels
custom_labels <- pft_counts %>%
  mutate(label = paste(PFT, " (", count, " obs)", sep = ""))



(heat_map_density <- ggplot() +
    geom_hex(data = Trait_species_with_PFT, aes(x = Longitude, y = Latitude, fill = PFT), bins = 30) +
    geom_sf(data = africa, fill = NA, color = "black") +  # Add country borders
    coord_sf() +  # Use coord_sf instead of coord_quickmap
    theme_void() +
    labs(x = "Longitude", y = "Latitude", fill = "PFT") +
    scale_fill_viridis_d()) + 
  scale_fill_viridis_d(labels = custom_labels$label) +  # Add custom labels to the legend
  ggtitle("Geographic distribution of PFTs in Africa")




# 3. Density Plot of Traits by PFT----
# allowing for a comparison of the distribution of trait values between different PFTs.

ggplot(Trait_species_with_PFT, aes(x = StdValue, fill = PFT)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ TraitName, scales = "free") +
  labs(title = "Density Plot of Traits by PFT", x = "Standard Value") +
  theme_minimal() + scale_x_log10() +
  scale_fill_viridis_d()




# 4. PFT vs Traits----
# create faceted scatter plots or box plots to visualize the relationship between PFT and various traits.
# Help understand the variability and central tendency of each trait within each PFT

# Scatter plot for Standard Value by PFT, faceted by TraitName
ggplot(Trait_species_with_PFT, aes(x = PFT, y = StdValue, color = PFT)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ TraitName, scales = "free_y") +
  theme_minimal() +
  labs(title = "Standard Value vs. PFT by Trait", x = "PFT", y = "Standard Value") + 
  scale_y_log10() +
  theme(legend.position = "none")
  


# Box plot of Standard Value by PFT, faceted by TraitName
ggplot(Trait_species_with_PFT, aes(x = PFT, y = StdValue, fill = PFT)) +
  geom_boxplot() +
  facet_wrap(~ TraitName, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Standard Value by PFT and Trait", x = "PFT", y = "Standard Value") + 
  scale_y_log10() +
  theme(legend.position = "none") +
  scale_fill_viridis_d()





# 5. Trait Distributions by PFT----
# Histogram or density plots showing the distribution of each trait for different PFTs.

# Histogram
ggplot(Trait_species_with_PFT, aes(x = StdValue, fill = PFT)) +
  geom_histogram(bins = 30, position = "dodge", alpha = 0.7) +
  facet_wrap(~ TraitName, scales = "free") +
  theme_minimal() +
  labs(title = "Trait Distributions by PFT", x = "Standard Value", y = "Count") +
  scale_fill_viridis_d()




# 6. Summary Statistics by PFT ----
#You could also visualize summary statistics like means and standard deviations of traits by PFT using bar plots.

# Summary statistics
summary_stats <- Trait_species_with_PFT %>%
  group_by(PFT, TraitName) %>%
  summarise(mean = mean(StdValue, na.rm = TRUE),
            sd = sd(StdValue, na.rm = TRUE))

# Plot
ggplot(summary_stats, aes(x = PFT, y = mean, fill = TraitName)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Mean Trait Values by PFT", x = "PFT", y = "Mean Value") +
  scale_fill_viridis_d()


# 7. Species diversity curve by PFT ----
# Prepare the data: Calculate the number of observations per PFT
pft_count <- Trait_species_with_PFT %>%
  group_by(PFT) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plotting: Rank PFTs on the x-axis and plot against their counts
ggplot(pft_count, aes(x = reorder(PFT, -count), y = count, group = 1)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.5) +  # Adjust 'span' to control smoothness
  labs(title = "Species accumulation curve",
       x = "PFT",
       y = "Observation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Calculate quantiles for the rank (x-axis) positions
quantiles <- quantile(1:nrow(pft_count), probs = c(0.25, 0.5, 0.75))

# Plotting: Rank PFTs on the x-axis and plot against their counts
ggplot(pft_count, aes(x = reorder(PFT, -count), y = count, group = 1)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.5) +  # Smooth curve
  geom_vline(xintercept = quantiles, linetype = "dashed", color = "blue") +  # Vertical quantile lines
  labs(title = "Species accumulation curve",
       x = "PFT",
       y = "Observation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# 8 Facet grid Trait distribution by PFT ----
# Summarize the data
trait_distribution <- Trait_species_with_PFT %>%
  group_by(PFT, TraitName) %>%
  summarise(count = n())

# Plot using facet grid
ggplot(trait_distribution, aes(x = PFT, y = count, fill = TraitName)) +
  geom_bar(stat = "identity") +
  labs(title = "Trait Distribution by PFT",
       x = "PFT",
       y = "Count of Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  facet_wrap(~ TraitName, scales = "free_y", nrow = 2, ncol = 4) +
  scale_fill_viridis_d()



# 9. Calculate the mean or median of StdValue for each PFT and TraitName----
pft_lines <- Trait_species_with_PFT %>%
  group_by(PFT, TraitName) %>%
  summarise(mean_value = mean(StdValue, na.rm = TRUE))

# Plot with density and PFT lines
ggplot(Trait_species_with_PFT, aes(x = StdValue)) +
  geom_density(fill = "red", alpha = 0.7) +
  facet_wrap(~TraitName, scales = "free", ncol = 4, labeller = label_wrap_gen(width = 30)) +
  geom_vline(data = pft_lines, aes(xintercept = mean_value, color = PFT), linetype = "solid") +
  labs(title = "Density Plot for Traits in Africa with PFT Lines",
       x = "Standardized Value",
       y = "Density") +
  theme_minimal() +
  scale_x_log10() +
  theme(legend.position = "bottom", panel.grid = element_blank())




# Alternative to saveplot to computer. ----

# Save the plot with increased size and resolution
# ggsave(map_plot, filename="plot_name.png", dpi=300, width=10, height=8)


#-----



# the remainder from the growth form.

#PFT_Remnant <- matched_data %>%
  #filter(PlantGrowthForm %in% c("herb", "fern"))

# combine this to the NA from PFT_Tree, NA from PFT_Tree_Broadleaf, NA from PFT_Needleleaf,
# NA from PFT_Shrub, NA from PFT_Grass





