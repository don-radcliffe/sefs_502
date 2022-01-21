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
