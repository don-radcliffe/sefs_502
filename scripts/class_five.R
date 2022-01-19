library(dplyr)

Oak <- read.csv("data/Oak_data_47x216.csv", header = TRUE,
                row.names = 1)
Oak_species <- read.csv("data/Oak_species_189x5.csv",
                        header = TRUE) 

Oak_abund <- Oak[ , colnames(Oak) %in% Oak_species$SpeciesCode]

library(vegan)
geog <- Oak[,c("LatAppx","LongAppx")]
geog.st <- decostand(x = geog, method = "range")
geog.dis <- vegdist(x = geog.st, method="euc") 

geog.dis <- Oak %>%
  dplyr::select(LatAppx, LongAppx) %>%
  ## decostand is a relativization function
  decostand(method = 'range') %>%
  vegdist(method = 'euclidean')
