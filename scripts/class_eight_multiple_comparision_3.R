perm.eg <- read.csv('data/permutation.example.csv', header = TRUE, row.names = 1)

source('scripts/load.oak.data.R')
grazing <- Oak$GrazCurr

grazing.result.adonis2 <- adonis2(Oak1 ~ grazing,
                                  method = "bray")
grazing.result.adonis2

Grazing <- factor(with(Oak, paste(GrazPast, GrazCurr, sep = "_")))
summary(Grazing)

adonis2(formula = Oak1 ~ Grazing, method = 'bray')

## pairwise adonis script
source('scripts/pairwise.adonis.R')
pairwise.adonis(resp = vegdist(Oak1), fact = Grazing)

Graz.res.betadisper = betadisper(d = oak1.dist, group = Grazing, type = 'centroid')

##### RRPP #####
install.packages('RRPP')
library(RRPP)

perm.eg.rrpp <- rrpp.data.frame(
  resp.dist = dist(perm.eg[ , c("Resp1", "Resp2")]),
  Group = perm.eg$Group)

  Oak.rrpp <- rrpp.data.frame(
    Oak1.dist = vegdist(Oak1),
    GrazPast = Oak$GrazPast ,
    GrazCurr = Oak$GrazCurr,
    Grazing)
str(Oak.rrpp)  

perm.eg.fit <- lm.rrpp(resp.dist ~ Group, data = perm.eg.rrpp)
  anova(perm.eg.fit)

