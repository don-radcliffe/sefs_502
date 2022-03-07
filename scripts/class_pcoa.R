## Ordination 4

require(vegan)

source('scripts/load.oak.data.R')
Oak1_PCoA <- wcmdscale(d = Oak1.dist, eig = TRUE)
print(Oak1_PCoA)
str(Oak1_PCoA)

library(ggplot2)
ggplot(data = data.frame(Oak1_PCoA$points),
       aes(x = Dim1, y = Dim2)) +
  geom_point() +
  theme_bw()
ggsave("graphics/PCoA.png", width = 5, height = 3,
       units = "in", dpi = 600)

geog.stand <- decostand(Oak[ , c("LatAppx", "LongAppx",
                                 "Elev.m")], "range")

Oak1_dbrda <- dbrda(Oak1 ~ Elev.m + LatAppx + LongAppx,
                      data = geog.stand, distance = "bray")

print(Oak1_dbrda)
