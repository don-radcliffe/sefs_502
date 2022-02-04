## sefs 502 group comparisons four
source("scripts/load.oak.data.R")

Grazing <- factor(with(Oak, paste(GrazPast, GrazCurr,
                                  sep = "_")))
summary(Grazing)

perm1 <- shuffle(n = length(Grazing))
perm1
perm2 <- shuffle(n = length(Grazing))
perm2

Oak_explan[ , "SppRich"]
Oak_explan[perm1, "SppRich"]

perms <- shuffleSet(n = length(Grazing), nset = 5); perms
str(perms)
class(perms)

shuffle(10)
shuffle(10)
set.seed(42); shuffle(10)
set.seed(42); shuffle(10) #identical to previous
set.seed(40); shuffle(10) #different
set.seed(NULL) #reset random number generator

adonis2(Oak1 ~ Grazing, method = 'bray', permutations = 999)
adonis2(Oak1 ~ Grazing, method = 'bray', permutations = perms)

CTRL <- how(within = Within(type = "free"),
            plots = Plots(type = "none"),
            nperm = 9999, observed = TRUE)
CTRL
check(Grazing, control = CTRL)

CTRL <- how(within = Within(type = "free"),
            plots = Plots(type = "none"),
            nperm = 99, observed = TRUE)
adonis2(Oak1.dist ~ Grazing, permutations = CTRL)
Graz.res.betadisper <- betadisper(d = Oak1.dist,
                                  group = Grazing, type = "centroid")
permutest(Graz.res.betadisper, pairwise = TRUE,
          permutations = CTRL)
