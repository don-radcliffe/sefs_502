perm.eg <- read.csv("data/Permutation.example.csv",
                    header = TRUE, row.names = 1)

library(ggplot2)
ggplot(data = perm.eg, aes(x = Resp1, y = Resp2)) +
  geom_point(aes(colour = Group, shape = Group), size = 5) +
  labs(title = "Real Data") +
  theme_bw()
ggsave("graphics/main.png",
       width = 3, height = 2.5,
       units = "in", dpi = 300)

perm.eg$perm1 <- sample(perm.eg$Group)
ggplot(data = perm.eg,
       aes(x = Resp1, y = Resp2)) +
  geom_point(aes(colour = perm1,
                 shape = perm1), size = 5) +
  labs(title = "Permutation 1") +
  theme_bw()
ggsave("graphics/perm1.png",
       width = 3, height = 2.5,
       units = "in", dpi = 300)

## you can run a script from another script using the source function
## this runs the load.oak.data script
source('scripts/load.oak.data.R')
str(Oak)

Resp.dist <- perm.eg %>%
  dplyr::select(Resp1, Resp2) %>%
  dist()
Resp.dist %>% round(3)

anosim(x, grouping, permutations = 999,
       distance = "bray", strata = NULL,
       parallel = getOption("mc.cores"))



grazing.results.anosim <- anosim(x = Oak1.dist,
                                 grouping = grazing, permutations = 999)
grazing.results.anosim

R.values <- with(grazing.results.anosim,
                 data.frame(R = c(statistic, perm) ) )
R.values$Type <- c("actual", rep("perm", length(R.values$R) -1))


ggplot(data = R.values, aes(x = R)) +
  geom_density() +
  geom_vline(data = R.values[R.values$Type == "actual" , ],
             aes(xintercept = R), colour = "red") +
  theme_bw()
ggsave("graphics/ANOSIM.R.png",
       width = 3, height = 2.5,
       units = "in", dpi = 300)