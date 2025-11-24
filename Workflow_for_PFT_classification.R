# Load packages and library----
library(tidyverse)
library(viridis)  
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(dplyr)



# Data load and preparation ----

Trait_species <- read_csv("Harmonized_spp.csv")  # trait data from TRY, containing the species for parameter assignment and PFT mapping

new_pft_class <- read_csv("new_pft_class.csv")   # Other data from csv - This data is the list of species in the trait data not found in the categorical table. 
                                                # parameter assignment and PFT classification was done manually outside R

Categorical_table <- read_csv("TRY_Categorical_Traits_Lookup_Table.csv") # TRY Categorical Look up table

vars <- c("AccSpeciesID", "AccSpeciesName", "Genus", 
          "SpeciesEpithet", "Family", "PlantGrowthForm",
          "LeafType", "LeafPhenology", "PhotosyntheticPathway")    # Keep only the columns needed for the categorical look up table

Categorical_table<- Categorical_table %>% dplyr::select(one_of(vars))


Categorical_table <- Categorical_table %>%
  filter(!(AccSpeciesID %in% c(25135, 62840)))    # Remove rows where AccSpeciesID is 25135 or 62840 - these are duplicates. 
                                                  #There may be duplicates in the categorical table, but only these 2 species occur in the trait data, so I have focused on them only


Categorical_table <- Categorical_table %>%
  rename(scientificName = AccSpeciesName)



## Sort the unique species from my working data ----

species_count_traitdata <- Trait_species %>%   # Count the occurrences of each species from my observation
  group_by(scientificName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


species_count_TRY <- Categorical_table %>%  # for categorical table data
  group_by(scientificName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))




## Merge the datasets based on species  ----

merged_data <- merge(species_count_traitdata, Categorical_table, by = "scientificName", all.x = TRUE)  # The resulting merged_data will contain all rows from species_count_traitdata and only the matching rows from categorical_table

matched_data <- merge(species_count_traitdata, Categorical_table, by = "scientificName")      # to keep only the species that are found in both datasets, use inner join

matched_data <- matched_data %>%
  mutate(ClimateZone = NA_character_) #add a column for climate zone


## Before moving forward, sort out the 4 parameters before assignment ----
classification_cols <- c("PlantGrowthForm", "LeafType", "LeafPhenology", "PhotosyntheticPathway")


# This is BEFORE – treating empty strings as missing
matched_data$param_count_before <- apply(matched_data[, classification_cols], 1, function(row) {
  sum(row != "" & !is.na(row))  # non-empty and non-NA
})


summary_table_before <- as.data.frame(table(matched_data$param_count_before))
names(summary_table_before) <- c("No_of_parameters", "No_of_species")
print(summary_table_before)




# Assign species to different classification parameter ----
## For plant growth form ----
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
                            "Vitex rivularis", "Xanthocercis zambesiaca", "Zanthoxylum zanthoxyloides", "Pausinystalia pynaertii", "Lonchocarpus laxiflorus", "Zanthoxylum gilletii", "Trichilia prieureana",
                            "Tessmannia lescrauwaetii", "Synsepalum stipulatum", "Synsepalum cerasiferum", "Scottellia klaineana", "Pterygota bequaertii", "Protorhus longifolia", "Prioria balsamifera",
                            "Pericopsis elata", "Pachyelasma tessmannii","Odyendea gabunensis", "Myrsine melanophloeos", "Lovoa trichilioides", "Guibourtia tessmannii", "Gambeya lacourtiana",
                            "Gambeya boiviniana", "Fillaeopsis discophora", "Autranella congolensis")




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
                             "Mezoneuron benthamianum", "Microglossa pyrifolia", "Adenia mcdadiana", "Alafia microstylis", "Silene succulenta", "Cissus verticillata", "Trichilia rubescens",
                             "Thilachium africanum", "Oxyanthus speciosus", "Osyris compressa", "Olax subscorpioidea")


## For Leaf type ----
matched_data$LeafType[is.na(matched_data$LeafType)] <- "broadleaved"


## For leaf phenology ----
species_to_update1 <- c("Baikiaea plurijuga", "Brachystegia longifolia", "Brachystegia spiciformis", 
                        "Bridelia ferruginea", "Cassia sieberiana", "Combretum glutinosum", "Combretum micranthum", 
                        "Combretum nigricans", "Crossopteryx febrifuga", "Detarium microcarpum", "Dioscorea bulbifera", 
                        "Faidherbia albida", "Flueggea virosa", "Gmelina arborea", "Lannea velutina", 
                        "Lonchocarpus laxiflorus", "Lophira lanceolata", "Milicia excelsa", "Peltophorum africanum", 
                        "Prosopis africana", "Quercus faginea", "Sterculia tragacantha", "Ximenia americana", "Albizia chinensis", "Securidaca longepedunculata")



species_to_update2 <- c("Aspidosperma megalocarpon", "Bauhinia petersiana", "Berchemia zeyheri", "Carapa procera", 
                        "Crassula rupestris", "Gardenia ternifolia", "Isoberlinia doka", "Neoboutonia macrocalyx", 
                        "Poulsenia armata", "Schotia afra", "Scolopia zeyheri", "Strychnos pungens", "Cordia megalantha", "Zygophyllum prismatocarpum")



matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(scientificName %in% c("Pouteria pierrei",
                                                      "Strophanthus hispidus",
                                                      "Strophanthus sarmentosus") &
        LeafPhenology == "evergreen", "deciduous", LeafPhenology))



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
                        "Vitex keniensis", "Halogeton alopecuroides", "Stephania cyanantha", "Mezoneuron benthamianum", "Zanthoxylum gilletii", "Thilachium africanum",
                        "Pterygota bequaertii", "Pericopsis elata") 




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
                        "Lycium shawii", "Adenia cissampeloides", "Cissampelos owariensis", "Pausinystalia pynaertii", "Adenia mcdadiana", "Trichilia prieureana",
                        "Tessmannia lescrauwaetii", "Synsepalum stipulatum", "Synsepalum cerasiferum", "Scottellia klaineana", "Protorhus longifolia", "Prioria balsamifera",
                        "Pachyelasma tessmannii", "Oxyanthus speciosus", "Osyris compressa", "Olax subscorpioidea", "Odyendea gabunensis", "Myrsine melanophloeos", "Lovoa trichilioides",
                        "Guibourtia tessmannii", "Gambeya lacourtiana", "Gambeya boiviniana", "Fillaeopsis discophora", "Autranella congolensis")




climateTr_species <- c("Acacia auriculiformis", "Acalypha glabrata", "Adenia kigogoensis", "Adenia litoralis", "Afrostyrax lepidophyllus",
                       "Afzelia africana", "Afzelia bipindensis", "Agarista salicifolia", "Aidia micrantha", "Albizia adianthifolia",
                       "Albizia gummifera", "Alchornea cordifolia", "Alchornea laxiflora", "Allanblackia floribunda", "Allanblackia stuhlmannii", 
                       "Allophylus africanus", "Allophylus pervillei", "Aloe littoralis", "Aloe thraskii", "Alstonia boonei", "Amphitecna tuxtlensis",
                       "Andira inermis", "Angylocalyx pynaertii", "Anisophyllea boehmii", "Annona senegalensis", "Anonidium mannii", "Anthocleista grandiflora", 
                       "Anthonotha fragrans", "Anthonotha macrophylla", "Antiaris toxicaria", "Antidesma membranaceum", "Antidesma vogelianum", "Aphloia theiformis", 
                       "Apodocephala pauciflora", "Apodytes dimidiata", "Aspidosperma megalocarpon", "Asteranthe asterias", 
                       "Aulacocalyx jasminiflora", "Autranella congolensis", "Baikiaea insignis", "Baikiaea robynsii", "Balanites aegyptiaca", "Balanites wilsoniana", 
                       "Baphia massaiensis", "Baphia wollastonii", "Barteria fistulosa", "Bauhinia petersiana", "Beilschmiedia velutina", "Bellucia grossularioides", 
                       "Berchemia zeyheri", "Berlinia auriculata", "Bersama abyssinica", "Blighia sapida", "Blighia welwitschii", "Boscia coriacea", "Boscia mossambicensis", 
                       "Boscia oleoides", "Boscia salicifolia", "Boscia senegalensis", "Bourreria petiolaris", "Brachylaena huillensis", "Brachystegia floribunda", 
                       "Bridelia cathartica", "Bridelia tulasneana", "Brosimum alicastrum", "Caloncoba welwitschii", "Calotropis procera", "Canarium madagascariense", 
                       "Canarium schweinfurtii", "Canthium inerme", "Canthium oligocarpum", "Carapa procera", "Carica papaya", "Casearia battiscombei", "Cassipourea euryoides", 
                       "Cassipourea gummiflua", "Cassipourea malosana", "Cassipourea mollis", "Cassipourea ruwensorensis", "Catunaregam nilotica", "Cavacoa quintasii", 
                       "Cecropia obtusifolia", "Chassalia umbraticola", "Chytranthus carneus", "Citropsis daweana", "Clausena anisata", "Cleistanthus polystachyus", 
                       "Cleistanthus schlechteri", "Cleistopholis patens", "Coelocaryon preussii", "Coffea arabica", "Coffea liberica", "Coffea racemosa", "Cojoba arborea", 
                       "Cola acuminata", "Cola chlamydantha", "Cola congolana", "Cola gigantea","Cola greenwayi", "Cola lateritia", "Cola lepidota", "Cola minor", "Cola pachycarpa", 
                       "Cola rostrata", "Combretum collinum", "Combretum zeyheri", "Commiphora schimperi", "Copaifera mildbraedii", "Coptosperma nigrescens", "Coptosperma supra-axillare", 
                       "Cordia dentata", "Cordia megalantha", "Cordia monoica", "Cordyla africana", "Cornus volkensii", "Corymbia citriodora", "Corynanthe pachyceras", "Coula edulis", 
                       "Craibia brevicaudata", "Craibia zimmermannii", "Croton megalocarpus", "Croton pseudopulchellus", "Croton steenkampianus", "Cryptocarya thouvenotii", 
                       "Cryptosepalum exfoliatum", "Cussonia sphaerocephala", "Cussonia spicata", "Cylicodiscus gabunensis", "Cynometra alexandri", 
                       "Cynometra hankei", "Cynometra webberi", "Dalbergia lactea", "Daniellia oliveri", "Dasylepis integra", "Detarium senegalense", "Dialium englerianum", 
                       "Dialium guineense", "Dialium orientale", "Dialium pachyphyllum", "Dialium schlechteri", "Dichostemma glaucescens", "Didelotia africana", "Diospyros abyssinica", 
                       "Diospyros barteri", "Diospyros batocana", "Diospyros bipindensis", "Diospyros boala", "Diospyros canaliculata", "Diospyros cinnabarina", "Diospyros confertiflora", 
                       "Diospyros crassiflora", "Diospyros dendo", "Diospyros elliotii", "Diospyros gabunensis", "Diospyros hoyleana", "Diospyros inhacaensis", "Diospyros kirkii", 
                       "Diospyros lycioides", "Diospyros mannii", "Diospyros mespiliformis", "Diospyros natalensis", "Diospyros piscatoria", "Diospyros quiloensis", "Diospyros suaveolens", 
                       "Diospyros whyteana", "Diospyros zombensis", "Discoglypremna caloneura", "Distemonanthus benthamianus", "Dombeya kirkii", "Dovyalis abyssinica", "Dovyalis longispina", 
                       "Drypetes arguta", "Drypetes gerrardii", "Drypetes natalensis", "Drypetes paxii", "Drypetes reticulata", "Drypetes usambarica", "Dussia mexicana", "Ekebergia capensis", 
                       "Elaeis guineensis", "Embelia schimperi", "Englerophytum natalense", "Englerophytum oblanceolatum", "Entada louvelii", "Ephippiandra madagascariensis", 
                       "Eriocoelum microspermum", "Erismadelphus exsul", "Erythrina excelsa", "Erythrococca polyandra", "Erythrophleum ivorense", "Erythrophleum lasianthum", 
                       "Erythrophleum suaveolens", "Erythroxylum emarginatum", "Eucalyptus alba", "Eucalyptus grandis", "Eucalyptus saligna", 
                       "Eucalyptus sideroxylon", "Eucalyptus tereticornis", "Eucalyptus urophylla", "Euclea crispa", "Euclea divinorum", "Euclea natalensis", "Euclea racemosa", "Euclea undulata", 
                       "Eugenia bukobensis", "Eugenia woodii", "Euphorbia cuneata", "Faurea saligna", "Feretia apodanthera", "Ficus craterostoma", 
                       "Ficus oreodryadum", "Ficus sur", "Ficus thonningii", "Ficus tremula",  "Ficus yoponensis", "Filicium decipiens", "Fillaeopsis discophora", "Flacourtia indica", 
                       "Funtumia africana", "Funtumia elastica", "Galpinia transvaalica", "Gambeya boiviniana", "Gambeya lacourtiana", "Garcinia afzelii", 
                       "Garcinia buchananii", "Garcinia epunctata", "Garcinia gerrardii", "Garcinia livingstonei", "Garcinia mannii", "Garcinia ovalifolia", "Garcinia punctata", 
                       "Garcinia smeathmannii", "Garcinia volkensii", "Gardenia ternifolia", "Gilbertiodendron dewevrei", "Gliricidia sepium", "Greenwayodendron suaveolens", 
                       "Grevillea robusta", "Grewia bicolor", "Grewia forbesii", "Grewia holstii", "Grewia monticola", "Grewia similis", "Guarea guidonia", "Guibourtia coleosperma", 
                       "Guibourtia tessmannii", "Gymnosporia buxifolia", "Gymnosporia mossambicensis", "Haplocoelum inoploeum", "Harrisonia abyssinica", "Harungana madagascariensis", 
                       "Heinsenia diervilleoides", "Heisteria parvifolia", "Heliocarpus appendiculatus", "Hexalobus crispiflorus", "Hippobromus pauciflorus", "Holarrhena floribunda", 
                       "Hunteria zeylanica", "Hylodendron gabunense", "Hymenaea verrucosa", "Hymenocardia ulmoides", "Hymenostegia pellegrinii", "Hypericum revolutum", "Hyphaene thebaica", 
                       "Hypodaphnis zenkeri", "Ilex mitis", "Inga sinacae", "Inhambanella henriquezii", "Irvingia gabonensis", "Irvingia robur", "Isoberlinia doka", "Isoberlinia scheffleri", 
                       "Isolona thonneri", "Julbernardia paniculata", "Julbernardia seretii", "Keetia cornelia", "Keetia zanzibarica", "Khaya ivorensis", "Khaya senegalensis", 
                       "Kigelia africana", "Labramia louvelii", "Landolphia buchananii", "Landolphia owariensis", "Lasianthus kilimandscharicus", "Lasiodiscus mannii", "Lecaniodiscus cupanioides", 
                       "Lepidotrichilia volkensii", "Lepisanthes senegalensis", "Leptaulus daphnoides", "Leptonychia usambarensis", "Leucaena leucocephala", "Librevillea klainei", "Lonchocarpus capassa", 
                       "Lophira alata", "Lovoa trichilioides", "Macaranga barteri", "Macaranga conglomerata", "Macaranga kilimandscharica", "Maerua crassifolia", "Maerua kirkii", "Mammea africana", 
                       "Mammea bongo", "Manilkara mochisia", "Manilkara sansibarensis", "Manilkara sulcata", "Mansonia altissima", "Maranthes glabra", "Mareya micrantha", "Markhamia lutea", 
                       "Marquesia macroura", "Maytenus procumbens", "Medusandra richardsiana", "Milicia regia", "Millettia dura", "Millettia ferruginea", "Mimusops obovata", "Mimusops obtusifolia", 
                       "Mimusops zeyheri", "Monanthotaxis fornicata", "Monanthotaxis parvifolia", "Monodora grandidieri", "Monodora myristica", "Monotes africanus", "Monotes glaber", 
                       "Morinda lucida", "Musanga cecropioides", "Myrianthus arboreus", "Mystroxylon aethiopicum", "Napoleonaea imperialis", "Nauclea diderrichii", "Nauclea latifolia", "Neoboutonia macrocalyx", 
                       "Nesogordonia holtzii", "Newbouldia laevis", "Newtonia hildebrandtii", "Nuxia capitata", "Nuxia congesta", "Ochna inermis", "Ochna natalitia", "Ochna pulchra", "Ochna schweinfurthiana", 
                       "Ochroma pyramidale", "Ocotea auriculiformis", "Ocotea gabonensis", "Ocotea racemosa", "Octoknema borealis", "Odyendea gabunensis", "Olea europaea", "Olea exasperata", "Olea woodiana", 
                       "Omphalocarpum elatum", "Oncinotis tenuiloba", "Oncoba routledgei", "Oncostemum botryoides", "Ongokea gore", "Ophiobotrys zenkeri", "Opilia campestris", "Ozoroa engleri", "Ozoroa paniculosa", 
                       "Pachyelasma tessmannii", "Pancovia golungensis", "Pancovia turbinata", "Panda oleosa", "Pappea capensis", "Paramacrolobium coeruleum", "Paraserianthes lophantha", "Parinari curatellifolia", 
                       "Parinari excelsa", "Pentaclethra macrophylla", "Pentadesma grandifolia", "Persea americana", "Petersianthus macrocarpus", "Phyllanthus reticulatus", "Piliostigma reticulatum", 
                       "Piptadeniastrum africanum", "Pittosporum undulatum", "Pleiocarpa pycnantha", "Pleurostylia africana", "Poga oleosa", "Polyscias albersiana", "Polyscias fulva", "Portulacaria afra", 
                       "Poulsenia armata", "Pouteria sapota", "Premna maxima", "Prioria balsamifera", "Prioria oxyphylla", "Protomegabaria stapfiana", "Prunus africana", "Psychotria capensis", "Pterocarpus santalinoides",
                       "Pterocarpus soyauxii", "Ptychopetalum petiolatum", "Pycnanthus angolensis", "Pyrostria bibracteata", "Rauvolfia vomitoria", "Rawsonia lucida", "Rhigozum zambesiacum", "Rinorea ilicifolia",
                       "Rinorea welwitschii", "Rothmannia engleriana", "Rothmannia globosa", "Rothmannia manganjae", "Rytigynia uhligii", "Saba comorensis", "Saba senegalensis", "Salacia leptoclada", "Salvadora persica", 
                       "Schotia afra", "Schotia brachypetala", "Scolopia zeyheri", "Scorodophloeus zenkeri", "Scottellia klaineana", "Scyphocephalium mannii", "Senna siamea","Sesbania sesban", "Sideroxylon inerme",
                       "Sloanea rhodantha", "Sorindeia madagascariensis", "Spathodea campanulata", "Spirostachys africana", "Staudtia kamerunensis", "Streblus dimepate", "Strephonema pseudocola", "Strombosia grandifolia", 
                       "Strombosia pustulata", "Strombosia scheffleri", "Strombosiopsis tetrandra", "Strychnos decussata", "Strychnos henningsii", "Strychnos pungens", "Suregada zanzibariensis", "Symphonia globulifera", 
                       "Synsepalum brevipes", "Synsepalum cerasiferum", "Synsepalum stipulatum", "Syzygium cordatum", "Syzygium guineense", "Syzygium jambos", "Syzygium parvifolium", "Syzygium sclerophyllum", 
                       "Tabernaemontana crassa", "Tabernaemontana pachysiphon","Tabernaemontana stapfiana", "Tamarindus indica", "Tambourissa thouvenotii", "Tarchonanthus camphoratus", 
                       "Terminalia sambesiaca", "Terminalia superba", "Tessmannia africana", "Tessmannia lescrauwaetii", "Tetraberlinia bifoliolata", "Tetrapleura tetraptera","Tetrorchidium didymostemon",
                       "Thespesia garckeana", "Thomandersia laurifolia", "Tiliacora funifera", "Treculia obovoidea", "Trichilia capitata", "Trichilia dregeana", "Trichilia emetica", "Trichilia monadelpha",
                       "Trichilia prieureana", "Trichilia welwitschii", "Trichoscypha patens", "Tridesmostemon omphalocarpoides", "Trilepisium madagascariense", "Turraea holstii", "Turraeanthus africanus", 
                       "Uapaca guineensis", "Uapaca kirkiana", "Uapaca louvelii", "Uapaca nitida", "Uapaca staudtii", "Umtiza listeriana", "Uvariopsis congensis", "Vepris lanceolata", "Vepris trichocarpa", 
                       "Vitellaria paradoxa", "Vitex grandifolia", "Vitex rivularis", "Vochysia guatemalensis", "Warburgia ugandensis", "Xanthocercis zambesiaca", "Xylopia aethiopica", 
                       "Xylopia arenaria","Xylopia odoratissima", "Xylopia quintasii", "Xylopia staudtii", "Xymalos monospora", "Zanthoxylum capense", "Zanthoxylum zanthoxyloides", "Ziziphus abyssinica",
                       "Anthyllis henoniana", "Brachylaena discolor", "Melanophylla crenata")



climateTe_species <- c("Acacia dealbata", "Acacia saligna", "Afrocarpus falcatus", "Brabejum stellatifolium", "Brachylaena neriifolia", 
                       "Buddleja saligna", "Burchellia bubalina", "Buxus macowanii", "Calodendrum capense", "Cunonia capensis", "Curtisia dentata", "Diospyros dichrophylla", 
                       "Elaeodendron croceum", "Eucalyptus cladocalyx", "Eucalyptus conferruminata", "Ficalhoa laurifolia", "Freylinia lanceolata", "Gonioma kamassi", 
                       "Halleria lucida", "Hyperacanthus amoenus", "Kiggelaria africana", "Leucosidea sericea", "Maytenus oleoides", "Myrsine melanophloeos", 
                       "Ochna arborea", "Ochna holstii", "Ocotea bullata", "Olea capensis", "Olinia rochetiana", "Olinia ventosa", "Platylophus trifoliatus", "Podocarpus elongatus",
                       "Podocarpus latifolius", "Protorhus longifolia", "Ptaeroxylon obliquum", "Pterocelastrus tricuspidatus", "Quercus ilex", "Quercus suber", "Rhamnus prinoides",
                       "Scolopia mundii", "Trichocladus crinitus", "Trichocladus ellipticus", "Virgilia oroboides", "Faurea macnaughtonii") 



## For Photosynthetic pathway C3 and C4 ----
matched_data <- matched_data %>%
  mutate(PhotosyntheticPathway = ifelse(scientificName =="Chenopodium album" & PhotosyntheticPathway == "C3/C4",
                                        "C3", PhotosyntheticPathway))


matched_data <- matched_data %>%
  mutate(PhotosyntheticPathway = ifelse(scientificName =="Euphorbia hirta" & PhotosyntheticPathway == "C3/C4",
                                        "C4", PhotosyntheticPathway))



species_to_update5 <- c("Agrostis kilimandscharica", "Festuca abyssinica", "Poa leptoclada", "Ehrharta stipoides", 
                        "Isachne mauritiana", "Pseudechinolaena polystachya", "Isachne mauritiana", "Poa cookii",
                        "Stipa parviflora", "Stipa tenacissima", "Pseudechinolaena polystachya", "Festuca obturbans",
                        "Actiniopteris radiata", "Asplenium friesiorum", "Dryopteris kilemensis", "Pteris catoptera",
                        "Selaginella kraussiana")



species_to_update6 <- c("Andropogon pinguipes", "Aristida adoensis", "Carex aethiopica", "Uncinia compacta", 
                        "Eragrostis patula",  "Cymbopogon pospischilii", "Digitaria abyssinica", 
                        "Digitaria pearsonii", "Tristachya biseriata", "Schmidtia kalahariensis", "Hyparrhenia confinis", 
                        "Chrysopogon plumulosus", "Chloris mossambicensis")




## bring together ----

matched_data <- matched_data %>%
  mutate(
    # ---- PlantGrowthForm ----
    PlantGrowthForm = case_when(
      scientificName %in% species_to_updateTrees  & is.na(PlantGrowthForm) ~ "tree",
      scientificName %in% species_to_updateShrubs & is.na(PlantGrowthForm) ~ "shrub",
      TRUE ~ PlantGrowthForm
    ),
    
    # ---- LeafPhenology ----
    LeafPhenology = case_when(
      # resolve "deciduous/evergreen"
      scientificName %in% species_to_update1 & LeafPhenology == "deciduous/evergreen" ~ "deciduous",
      scientificName %in% species_to_update2 & LeafPhenology == "deciduous/evergreen" ~ "evergreen",
      
      # fill in missing values
      scientificName %in% species_to_update3 & is.na(LeafPhenology) ~ "deciduous",
      scientificName %in% species_to_update4 & is.na(LeafPhenology) ~ "evergreen",
      
      TRUE ~ LeafPhenology
    ),
    
    # ---- ClimateZone ----
    ClimateZone = case_when(
      scientificName %in% climateTr_species & is.na(ClimateZone) ~ "tropical",
      scientificName %in% climateTe_species & is.na(ClimateZone) ~ "temperate",
      TRUE ~ ClimateZone
    ),
    
    # ---- PhotosyntheticPathway ----
    PhotosyntheticPathway = case_when(
      scientificName %in% species_to_update5 & is.na(PhotosyntheticPathway) ~ "C3",
      scientificName %in% species_to_update6 & is.na(PhotosyntheticPathway) ~ "C4",
      TRUE ~ PhotosyntheticPathway
    )
  )




count_levels <- function(df, col) {
  df %>%
    count({{ col }}, name = "count") %>%
    arrange(desc(count))
}

PlantGrowthForm        <- count_levels(matched_data, PlantGrowthForm)
LeafType               <- count_levels(matched_data, LeafType)
LeafPhenology          <- count_levels(matched_data, LeafPhenology)
PhotosyntheticPathway  <- count_levels(matched_data, PhotosyntheticPathway)



# Combine both dataset - combining the matched dataset with complete information and the other data that was sorted outside of R to get the complete species data collected for this study ----

matched_data <- bind_rows(matched_data, new_pft_class)




# Before grouping, sort out the parameter assignment after ----

matched_data$param_count_after <- apply(matched_data[, classification_cols], 1, function(row) {
  sum(row != "" & !is.na(row))
})                                 # This is AFTER – counts what's now filled in (same as above, done assigning)


matched_data$param_count_before[is.na(matched_data$param_count_before)] <- 0    # Replace NA values in param_count_before with 0


summary_before <- matched_data %>%
  count(param_count_before) %>%
  rename(No_of_parameters = param_count_before, No_of_species = n) %>%
  mutate(Status = "Before")

summary_after <- matched_data %>%
  count(param_count_after) %>%
  rename(No_of_parameters = param_count_after, No_of_species = n) %>%
  mutate(Status = "After")

summary_combined <- bind_rows(summary_before, summary_after)


write.csv(summary_combined, "summary_before_after.csv", row.names = FALSE)        # save summary table




# Plot the before and after parameter assignment ----
summary_plot <- ggplot(summary_combined, aes(x = as.numeric(No_of_parameters), y = No_of_species, color = Status)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, method = "loess", span = 0.6, size = 1.2) +
  scale_x_continuous(breaks = 0:4) +
  scale_y_continuous(breaks = seq(0, 1000, by = 200)) +
  scale_color_manual(
    name = "Parameter information availability",
    values = c("Before" = "#00BFC4", "After" = "#F8766D"),
    labels = c("Before" = "Original dataset", "After" = "After parameter assignment")
  ) +
  labs(
    x = "Number of parameters present (out of 4)",
    y = "Number of species"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),       # Bigger legend title
    legend.text = element_text(size = 18),        # Bigger legend labels
    axis.line = element_line(size = 0.8, color = "black"),  # X and Y axis lines
    panel.grid.major = element_line(color = "grey90", size = 0.4), # Optional soft grid
    panel.grid.minor = element_blank()
  )



ggsave("summary_plot.png", plot = summary_plot, width = 16, height = 16, dpi = 300, bg = "white")       # save plot





# Group species into different PFT classes ----
# based on the following criteria LeafType, PlantGrowtForm, LeafPhenology, PhotosynthicPathway, ClimateZone (for BET)


matched_data <- matched_data %>%
  mutate(
    PFT = case_when(
      ## ---- Broadleaf evergreen trees ----
      PlantGrowthForm %in% c("tree", "shrub/tree") &
        LeafType == "broadleaved" &
        LeafPhenology == "evergreen" &
        ClimateZone == "tropical" ~ "BET-Tr",   # 1
      
      PlantGrowthForm %in% c("tree", "shrub/tree") &
        LeafType == "broadleaved" &
        LeafPhenology == "evergreen" &
        ClimateZone == "temperate" ~ "BET-Te",  # 2
      
      ## ---- Broadleaf deciduous trees ----
      PlantGrowthForm %in% c("tree", "shrub/tree") &
        LeafType == "broadleaved" &
        LeafPhenology == "deciduous" ~ "BDT",   # 3
      
      ## ---- Needleleaf evergreen trees ----
      PlantGrowthForm %in% c("tree", "shrub/tree") &
        LeafType %in% c("scale-shaped", "needleleaved") &
        LeafPhenology == "evergreen" ~ "NET",   # 4
      
      ## ---- Needleleaf deciduous trees ----
      PlantGrowthForm %in% c("tree", "shrub/tree") &
        LeafType %in% c("scale-shaped", "needleleaved") &
        LeafPhenology == "deciduous" ~ "NDT",   # 5
      
      ## ---- Shrubs ----
      PlantGrowthForm %in% c("shrub", "herb/shrub") &
        LeafPhenology == "evergreen" ~ "ESH",   # 6
      
      PlantGrowthForm %in% c("shrub", "herb/shrub") &
        LeafPhenology == "deciduous" ~ "DSH",   # 7
      
      ## ---- Grasses ----
      PlantGrowthForm == "graminoid" &
        PhotosyntheticPathway %in% c("C3", "C3/CAM") ~ "C3",  # 8
      
      PlantGrowthForm == "graminoid" &
        PhotosyntheticPathway %in% c("C4", "C4/CAM") ~ "C4",  # 9
      
      # fallback: keep existing PFT (if it exists) or NA
      TRUE ~ NA_character_
    )
  )


## Filter rows where PFT is NA. These were species that could not be classified ----
combined_df_PFT <- matched_data %>%
  filter(!is.na(PFT))



# Merge the datasets based on Species Name
# The resulting merged_data will contain all rows from combined_df_PFT and only the matching rows from Trait_species
species_with_PFT  <- merge(combined_df_PFT, Trait_species, by = "scientificName", all.x = TRUE)


# remove duplicates
species_with_PFT <- species_with_PFT %>%
  distinct(scientificName, .keep_all = TRUE)


# clean dataset
species_with_PFT<- species_with_PFT %>%
  dplyr::select(
    -c(count, AccSpeciesID, Genus, SpeciesEpithet, 
      Family, count.x, count.y, spec.name))


# sort out the family name for each classified species
family_count_traitdata <- species_with_PFT %>%
  group_by(family) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 



# Plot PFT occurrence ----
# Prepare data
pft_species_count <- species_with_PFT %>%
  group_by(PFT) %>%
  summarise(SpeciesCount = n_distinct(scientificName)) %>%
  mutate(PFT_Full = case_when(
    PFT == "BDT" ~ "Broadleaf Deciduous Trees",
    PFT == "BET-Tr" ~ "Tropical Broadleaf Evergreen Trees",
    PFT == "BET-Te" ~ "Temperate Broadleaf Evergreen Trees",
    PFT == "C3" ~ "C3 Grasses",
    PFT == "C4" ~ "C4 Grasses",
    PFT == "DSH" ~ "Deciduous Shrubs",
    PFT == "ESH" ~ "Evergreen Shrubs",
    PFT == "NET" ~ "Needleleaf Evergreen Trees",
    TRUE ~ PFT
  )) %>%
  arrange(desc(SpeciesCount)) %>%
  mutate(index = row_number())  # numeric x-axis





# Plot using numeric index to control spacing
bar_chart <- ggplot(pft_species_count, aes(x = index, y = SpeciesCount)) +
  geom_col(width = 0.9, fill = "grey30") +
  scale_x_continuous(breaks = pft_species_count$index, labels = pft_species_count$PFT_Full) +
  labs(
    x = "Plant Functional Types (PFTs)",
    y = "Number of Unique Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 22),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold"),
    plot.margin = margin(1, 1, 1.5, 1, "cm")
  )


ggsave("new_PFT_species_fixed_spacing.png", plot = bar_chart, width = 16, height = 16, dpi = 300, bg = "white")


write.csv(species_with_PFT, "PFT_mapped_data.csv", row.names = FALSE)





                                                       # ----------------- END ----------------- #

