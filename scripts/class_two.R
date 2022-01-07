Oak <- read.csv("data/Oak_data_47x216.csv", header = TRUE,
                row.names = 1)
Oak_species <- read.csv("data/Oak_species_189x5.csv",
                        header = TRUE)
#Create separate objects for the response and explanatory data:
  Oak_abund <- Oak[ , colnames(Oak) %in% Oak_species$SpeciesCode]
Oak_explan <- Oak[ , ! colnames(Oak) %in%
                     Oak_species$SpeciesCode]
head(Oak_explan)
