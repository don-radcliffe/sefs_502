## Ordination 5 nmds

source("scripts/load.oak.data.R")

Oak0 <- decostand(Oak_abund, "max")

quick.metaMDS <- function(dataframe, dimensions) {
  metaMDS(comm = dataframe, autotransform = FALSE,
          distance = "bray", engine = "monoMDS", k = dimensions,
          weakties = TRUE, model = "global", maxit = 300,
          try = 40, trymax = 100, wascores = TRUE)
}

Oak1.z <- quick.metaMDS(dataframe = Oak1, dimensions = 3)
Oak0.z <- quick.metaMDS(dataframe = Oak0, dimensions = 3)

## Procrustes analysis
rare.v.norare <- protest(Oak1.z, Oak0.z)
print(rare.v.norare)

## Permutations of zero takes away statistical tests
geog.fit <- envfit(Oak1.z ~ Elev.m + LatAppx + LongAppx,
                   data = Oak, permutations = 0)
geog.fit

plot(Oak1.z, display = 'sites')
plot(geog.fit)

ordisurf(Oak1.z, Oak$AHoriz,
         data = Oak, permutations = 0)
plot(geog.fit)

Oak1.z.elev <- MDSrotate(Oak1.z, Oak$Elev.m)
plot(Oak1.z.elev, display = 'sites')
plot(envfit(Oak1.z.elev ~ Elev.m, data = Oak, elevations = 0))
