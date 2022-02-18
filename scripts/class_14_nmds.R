source("scripts/load.oak.data.R")

z <- metaMDS(comm = Oak1, autotransform = FALSE,
             distance = "bray", engine = "monoMDS", k = 3, weakties = TRUE,
             model = "global", maxit = 300, try = 40, trymax = 100)
z
print(z)

gof <- goodness(object = z)
gof

plot(z, display = "sites", type = "none")
points(z, display = "sites", cex = 2*gof/mean(gof))

z$stress
plot(z$diss, z$dist)
plot(vegdist(Oak1), dist(scores(z))) #equivalent

stressplot(object = z, lwd = 5)

z$points

z.points <- data.frame(z$points)

p <- ggplot(data = z.points, aes(x = MDS1, y = MDS2)) +
  geom_point() +
  ggtitle('Oak nmds') +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())
p
ggsave("graphics/NMDS.png", width = 5, height = 3,
       units = "in", dpi = 600)

p + geom_text(label = rownames(z.points), colour = "red")

sp <- wascores(x = z$points, w = Oak1, expand = TRUE)
Quga <- data.frame(sp[c("Quga.s", "Quga.t"), ])
p + geom_text(data = Quga, label = rownames(Quga))

z.points <- data.frame(z.points,
                       Quga.s = Oak1$Quga.s,
                       Quga.t = Oak1$Quga.t)

ggplot(data = z.points, aes(x = MDS1, y = MDS2)) +
  geom_text(data = Quga, label = rownames(Quga),
            colour = c("red", "blue")) +
  geom_point(data = z.points[z.points$Quga.s > 0,],
             aes(size = Quga.s), shape = 21, colour = "red") +
  geom_point(data = z.points[z.points$Quga.t > 0,],
             aes(size = Quga.t), shape = 21, colour = "blue") +
  labs(size = "Abundance") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()) +
  ggtitle('Oak nmds')
ggsave("graphics/NMDS.Quga.png", width = 6, height = 3,
       units = "in", dpi = 600)
