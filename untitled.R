matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName == "Albizia chinensis" & LeafPhenology == "deciduous/evergreen", 
                                "deciduous", LeafPhenology))





matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName == "Tephrosia polystachya" & LeafPhenology == "", 
                                "evergreen", LeafPhenology))






#FOR THE ONES THAT ARE "DECIDOUS/EVERGREEN"
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










# FOR DECIDUOUS AND EVERGREEN
species_to_update3 <- c("Markhamia obtusifolia", "Monodora angolensis", "Terminalia avicennioides", 
                        "Terminalia brachystemma", "Terminalia mollis", "Terminalia prunioides", 
                        "Vepris zambesiaca", "Maerua angolensis", "Maerua prittwitzii", "Maesopsis eminii",
                        "Monodora junodii", "Nectaropetalum kaessneri", "Newtonia buchananii",
                        "Khaya grandifoliola", "Ozoroa insignis", "Flueggea virosa", "Aidia genipiflora",
                        "Khaya anthotheca", "Ficus sycomorus", "Combretum mossambicense", "Chaetachme aristata", 
                        "Cola griseiflora", "Albizia ferruginea", "Ficus lutea", "Brachylaena ramiflora", "Hagenia abyssinica",
                        "Bauhinia thonningii", "Ricinodendron heudelotii", "Lannea welwitschii", "Dalbergia armata", 
                        "Brachystegia boehmii", "Lannea schweinfurthii", "Cordia sinensis", "Combretum fruticosum",
                        "Bridelia scleroneura", "Isoberlinia angolensis", "Albizia harveyi", "Cleistochlamys kirkii",
                        "Commiphora neglecta", "Diospyros alboflavescens", "Entandrophragma candollei", "Grewia mollis", 
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
                        "Rubus apetalus")
                        
                        


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
                        "Phytolacca dodecandra", "Cyphostemma adenocaule", "Pisonia aculeata", "Rhaphiostylis beninensis", "Gardenia erubescens", "Apodostigma pallens")
                        


matched_data <- matched_data %>%
  mutate(LeafPhenology = ifelse(AccSpeciesName %in% species_to_update4 & LeafPhenology == "", 
                                "evergreen", LeafPhenology))












#FOR THE PLANT GROWTH FORM
species_to_updateTrees <- c("Vitellaria paradoxa", )

species_to_updateShrubs <- c("Lycium shawii", "Haloxylon scoparium", "Helianthemum lippii", "Gymnosporia senegalensis", "Koelpinia linearis")

species_to_updateGrasses <- c()

species_to_updateClimbers <- c()



#C3 AND C4
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
