## Class xx: followup tests: TITAN, ISA, SIMPER

source('scripts/load.oak.data.R')

head(Oak)
hist(Oak$Quga.t)

library(tidyverse)
Oak_explan <- Oak_explan %>%
  rownames_to_column(var = "Stand") %>%
  mutate(GP_GC = paste(GrazPast, GrazCurr, sep = "_")) %>%
  merge(y = data.frame(GP_GC = c("No_No", "Yes_No", "Yes_Yes"),
                       Grazing = c("Never", "Past", "Always"))) %>%
  arrange(Stand)
View(Oak_explan)

set.seed(42)
adonis2(Oak1 ~ Grazing, data = Oak_explan, distance = 'bray')

##SIMPER
simper.Grazing <- simper(comm = Oak1, group = Oak_explan$Grazing)
summary(simper.Grazing)

## Indicator species analysis ISA
require(indicspecies)

set.seed(42)

Graz.ISA <- multipatt(x = Oak1, cluster = Oak_explan$Grazing, duleg = TRUE)
summary(Graz.ISA, alpha = 0.1)

## Testing both the treatments and the combinations of treatments
set.seed(42)
Graz.ISA.2 <- multipatt(x = Oak1, cluster = Oak_explan$Grazing)
summary(Graz.ISA.2)

Graz.ISA.2$sign %>% head()
Graz.ISA.2$comb %>% head()

## TITAN does something similar based on continuous variables
## Regression tree plus ISA
ggplot(data = Oak_explan, aes(x = PDIR)) +
  geom_density() + theme_bw()

set.seed(42)
adonis2(Oak1~PDIR, data = Oak_explan, distance = 'bray')
titan.PDIR <- titan(env = Oak_explan$PDIR, txa = Oak1)

titan.PDIR$sppmax[ , c("zenv.cp", "freq", "maxgrp",
                       "IndVal", "purity", "reliability", "filter")]

as.data.frame(titan.PDIR$sppmax) %>%
  filter(filter != 0) %>%
  select(zenv.cp, freq, maxgrp, IndVal, purity, reliability) %>%
  arrange(maxgrp, zenv.cp)

## Some graphical options for TITAN
plot_taxa_ridges(titan.PDIR, xlabel = 'PDIR')
plot_cps(titan.PDIR, xlabel = 'PDIR')
plot_cps(titan.PDIR, xlabel = 'PDIR', taxaID = 'Hodi.s')
