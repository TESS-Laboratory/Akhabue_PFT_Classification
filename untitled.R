matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName == "Albizia chinensis" & LeafPhenology == "deciduous/evergreen", 
                                "deciduous", LeafPhenology))





matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName == "Tephrosia polystachya" & LeafPhenology == "", 
                                "evergreen", LeafPhenology))









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
  mutate(PhotosyntheticPathway = ifelse(AccSpeciesName =="Chenopodium album" & PhotosyntheticPathway == "C3/C4",
                                        "C3", PhotosyntheticPathway))


matched_data <- matched_data %>%
  mutate(PhotosyntheticPathway = ifelse(AccSpeciesName =="Euphorbia hirta" & PhotosyntheticPathway == "C3/C4",
                                        "C4", PhotosyntheticPathway))






species_to_update3 <- c("Markhamia obtusifolia", "Monodora angolensis", "Terminalia avicennioides", 
                        "Terminalia brachystemma", "Terminalia mollis", "Terminalia prunioides", 
                        "Vepris zambesiaca", "Maerua angolensis", "Maerua prittwitzii", "Maesopsis eminii",
                        "Monodora junodii", "Nectaropetalum kaessneri", "Newtonia buchananii",
                        "Khaya grandifoliola", "Ozoroa insignis", "Flueggea virosa", "Aidia genipiflora",
                        "Khaya anthotheca", "Ficus sycomorus")


matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName %in% species_to_update3 & LeafPhenology == "", 
                                "deciduous", LeafPhenology))



species_to_update4 <- c("Terminalia sambesiaca", "Terminalia sambesiaca", "Mammea bongo", "Manilkara sansibarensis", 
                        "Maranthes glabra", "Mareya micrantha", "Melanophylla crenata", "Mimusops obtusifolia", 
                        "Monodora grandidieri", "Morinda lucida", "Aphloia theiformis", "Trilepisium madagascariense", 
                        "Bridelia cathartica", "Gilbertiodendron dewevrei", "Tabernaemontana stapfiana", "Hypericum revolutum",
                        "Rhamnus prinoides", "Apodocephala pauciflora", "Olinia rochetiana", "Streblus dimepate", "Uapaca louvelii",
                        "Weinmannia rutenbergii", "Halleria lucida", "Anonidium mannii")


matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName %in% species_to_update4 & LeafPhenology == "", 
                                "evergreen", LeafPhenology))










#C4 AND C4
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
