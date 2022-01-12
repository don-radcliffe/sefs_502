Oak <- read.csv("data/Oak_data_47x216.csv", header = TRUE,
                row.names = 1)
Oak_species <- read.csv("data/Oak_species_189x5.csv",
                        header = TRUE)
#Create separate objects for the response and explanatory data:
  Oak_abund <- Oak[ , colnames(Oak) %in% Oak_species$SpeciesCode]
Oak_explan <- Oak[ , ! colnames(Oak) %in%
                     Oak_species$SpeciesCode]
head(Oak_explan)

install.packages('labdsv')
require('labdsv')
labdsv::vegtab()

vegtab(comm, set, minval = 1, pltord, spcord, pltlbl,
       trans = FALSE)

Oak_no_raw <- vegtab(comm = Oak_abund, minval = 0.05 * nrow(Oak_abund))
View(Oak_no_raw)
