##### COMPOSITIONAL ANALYSIS
## Virtanen Bryophyte Manuscript
## JD Bakker et al
## 211228

# 1.0 LOAD ITEMS -----------------------------------------------------------
# 1.1 Load packages ----
library(readxl)
library(plyr)
library(labdsv)
library(vegan)
library(tidyverse)
library(BiodiversityR)


# 1.2 Load functions ---------------------------------------------------------------
quick.ggsave <- function(filename, height = 3.25) {
  ggsave(filename, height = height, width = 6.5, units = "in", dpi = 600)
}

# 1.3 Load data ----
datafile <- "data/bank_vege_12max_2016_environmentaldata_211224.csv"

original.matrix <- read.csv(datafile) %>%
  mutate(PlotID = paste(Block, plot),
         Trial = ifelse(trial == "bank", "Diaspore Bank", "Vegetation"),
         trt = factor(trt, ordered = TRUE, levels = c("Control", "Fence", "NPK", "NPK+Fence") ) )

species.matrix <- original.matrix %>%
  dplyr::select(Ptychostomum_cf_moravicum:Syntrichia_norvegica)
species.names <- colnames(species.matrix)

species.matrix.total <- decostand(species.matrix, "total")

taxa <- read.csv("data/bryophyte.taxa.csv")


# 2.0 Rank abundance curves ----

bank.rank <- rankabundance(species.matrix, y = original.matrix, factor = "trial", level = "bank") %>%
  as.data.frame() %>%
  mutate(Trial = "Diaspore Bank")

vege.rank <- rankabundance(species.matrix, y = original.matrix, factor = "trial", level = "vege") %>%
  as.data.frame() %>%
  mutate(Trial = "Vegetation")

all.ranks <- rbind(bank.rank, vege.rank)

ggplot(data = all.ranks, aes(x = rank, y = proportion)) +
  geom_line() +
  geom_point() +
  facet_grid(cols = vars(Trial), space = "free_x", scales = "free_x") +
  lims(y = c(0,100)) +
  labs(x = "Rank", y = "Proportion") +
  theme_bw()
ggsave("graphs/Rank.Abundance.png", width = 6, height = 3, units = "in", dpi = 600)



# 3.0 Colonist vs Perennial ----
colonist.perennial <- original.matrix %>%
  dplyr::select(Trial, all_of(species.names)) %>%
  pivot_longer(cols = -Trial, names_to = "Taxon", values_to = "Cover") %>%
  #dplyr::select(Trial, Taxon, Cover) %>%
  group_by(Trial, Taxon) %>%
  summarize(Cover = sum(Cover), .groups = "keep") %>%
  merge(y = taxa, all.x = TRUE, all.y = FALSE) %>%
  group_by(Trial, Colonist.Perennial) %>%
  summarize(Richness = sum(Cover > 0), Cover = sum(Cover), .groups = "keep")
colonist.perennial

original.matrix %>%
  dplyr::select(all_of(species.names)) %>%
  pivot_longer(cols = all_of(species.names), names_to = "Taxon", values_to = "Cover") %>%
  group_by(Taxon) %>%
  summarize(Cover = sum(Cover), .groups = "keep") %>%
  merge(y = taxa, all.x = TRUE, all.y = FALSE) %>%
  group_by(Colonist.Perennial) %>%
  summarize(Richness = sum(Cover > 0), Cover = sum(Cover), .groups = "keep")


# 4.0 PERMANOVA of full compositional matrix (relativized by plot total) ----
(whole.plot <- adonis2(vegdist(species.matrix.total) ~ Block * NPK * Fence, data = original.matrix))
#statistical significance is incorrect - interactions with block should form the error term for F-statistics of NPK and Fence
#however, probably not necessary for this manuscript as interest is not in overall effect of factors

set.seed(42)
(split.plot <- adonis2(vegdist(species.matrix.total) ~ PlotID + trial + trial:NPK + trial:Fence + trial:NPK:Fence, data = original.matrix))
#PlotID term accounts for plot-to-plot variation; this includes variation among blocks
#trial is highly significant


# 4.1 NMDS ----
bryo.mds <- metaMDS(comm = species.matrix.total, 
                    autotransform = FALSE, wascores = TRUE)

original.matrix <- data.frame(original.matrix,
                              bryo.mds$points)
#colour.trt <- c("Control" = "grey", "Fence" = "blue",
#                "NPK" = "red", "NPK+Fence" = "dark red")
ggplot(data = original.matrix, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(colour = trt, shape = Trial)) +
  #scale_color_manual(values = colour.trt) +
  scale_color_brewer(type = "div", palette = "RdYlBu") +
  scale_shape_manual(values = c(16, 15)) +
  guides(colour = guide_legend(title = "Treatment"),
         shape = guide_legend(title = "Assemblage")) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 1)
ggsave("graphs/NMDS.png", width = 6.5, height = 3, units = "in", dpi = 600)


# 5.0 Indicator Species Analysis ----
library(indicspecies)
set.seed(42)
summary(multipatt(species.matrix, original.matrix$Trial))

temp <- original.matrix %>%
  pivot_longer(cols = all_of(species.names), names_to = "Taxon", values_to = "Cover") %>%
  filter(Taxon %in% c("Pohlia_nutans", "Sanionia_uncinata",
                      "Kiaeria_starkei", "Hylocomiastrum_pyrenaicum")) %>%
  mutate(trt = as.factor(trt))
ggplot(temp, aes(x = MDS1, y = MDS2)) +
  geom_point(data = temp[temp$Cover == 0 , ], aes(shape = Trial), 
             colour = "grey", size = 1) +
  geom_point(data = temp[temp$Cover > 0 , ], 
             aes(shape = Trial, colour = trt, size = Cover), alpha = 0.5) +
  scale_color_brewer(type = "div", palette = "RdYlBu") +
  scale_shape_manual(values = c(16, 15)) +
  guides(size = "none",
         colour = "none",
         shape = "none") +
  facet_grid(cols = vars(Taxon)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        aspect.ratio = 1)
ggsave("graphs/NMDS.species.png", width = 6.5, height = 2, units = "in", dpi = 600)


# 6.0 Total richness and cover ----
totals <- original.matrix %>%
  dplyr::select(PlotID, Trial, all_of(species.names)) %>%
  pivot_longer(cols = -c(PlotID, Trial), names_to = "Taxon", values_to = "Cover") %>%
  group_by(PlotID, Trial) %>%
  summarize(Richness = sum(Cover > 0), Cover = sum(Cover), .groups = "keep") %>%
  pivot_longer(cols = c(Richness, Cover), names_to = "Response", values_to = "Value") %>%
  merge(y = original.matrix[ , c("PlotID", "Trial", "Block", "NPK", "Fence", "trt")])

# lm-tests
totals.wide <- totals %>%
  pivot_wider(names_from = Response, values_from = Value)

# richness
anova(b_sr_lm <- lm(sqrt(Richness) ~ Block + Fence * NPK,
                    data = totals.wide[totals.wide$Trial == "Diaspore Bank" , ]))
b_sr_lm_letters <- data.frame(Trial = "Diaspore Bank",
                              Response = "Richness",
                              trt = 3.5,
                              Value = 20,
                              text = "NS")

anova(v_sr_lm <- lm(sqrt(Richness) ~ Block + Fence * NPK,
                    data = totals.wide[totals.wide$Trial == "Vegetation" , ]))
v_sr_lm_letters <- data.frame(Trial = "Vegetation",
                              Response = "Richness",
                              trt = 3.5,
                              Value = 20,
                              text = "B,F,N,F:N")

# cover
anova(b_tot_lm <- lm(asin(sqrt(Cover/100))~ Block + Fence * NPK,
                     data = totals.wide[totals.wide$Trial == "Diaspore Bank" , ]))
# doesn't work because 7 trays have values > 100%
b_tot_lm_letters <- data.frame(Trial = "Diaspore Bank",
                               Response = "Cover",
                               trt = 3.5,
                               Value = 130,
                               text = "NS")
anova(v_tot_lm <- lm(asin(sqrt(Cover/100))~ Block + Fence * NPK,
                     data = totals.wide[totals.wide$Trial == "Vegetation" , ]))
v_tot_lm_letters <- data.frame(Trial = "Vegetation",
                               Response = "Cover",
                               trt = 3.5,
                               Value = 130,
                               text = "N")

letters <- rbind(b_sr_lm_letters, v_sr_lm_letters,
                 b_tot_lm_letters, v_tot_lm_letters)

totals.mean <- totals %>%
  group_by(Trial, NPK, Fence, trt, Response) %>%
  summarize(Value = mean(Value), .groups = "keep")

ggplot(data = totals, aes(x = trt, y = Value)) +
  geom_point(data = totals.mean, shape = "-", col = "red", size = 10) +
  geom_point() +
  #annotate(data = letters, aes(label = text)) +
  geom_text(data = letters, aes(label = text), size = 3, hjust = "center") +
  facet_grid(rows = vars(Response), cols = vars(Trial), scales = "free_y") +
  labs(x = "Treatment", y = "Response") +
  theme_bw()
ggsave("graphs/Cover.Richness.png", width = 6, height = 3, units = "in", dpi = 600)


# 7.0 Taxonomic-Morphological Groups ----
taxon.groups <- original.matrix %>%
  dplyr::select(PlotID, Trial, all_of(species.names)) %>%
  pivot_longer(cols = -c(PlotID, Trial), names_to = "Taxon", values_to = "Cover") %>%
  merge(y = taxa, all.x = TRUE, all.y = FALSE) %>%
  group_by(PlotID, Trial, Group, TaxonomicMorphologicalGroup) %>%
  summarize(Cover = sum(Cover), .groups = "keep") %>%
  merge(y = original.matrix[ , c("PlotID", "Trial", "Block", "NPK", "Fence", "trt")])

temp <- taxon.groups[taxon.groups$Group == "Dicran" , ]
anova(b_dicran_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                     data = temp[temp$Trial == "Diaspore Bank" , ]))
b_dicran_lm_letters <- data.frame(Trial = "Diaspore Bank",
                               TaxonomicMorphologicalGroup = "Dicran.",
                               trt = 3.5,
                               Cover = 7,
                               text = "B")
anova(v_dicran_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                     data = temp[temp$Trial == "Vegetation" , ]))
v_dicran_lm_letters <- data.frame(Trial = "Vegetation",
                                  TaxonomicMorphologicalGroup = "Dicran.",
                                  trt = 3.5,
                                  Cover = 7,
                                  text = "N")

temp <- taxon.groups[taxon.groups$Group == "Liver" , ]
anova(b_liver_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                       data = temp[temp$Trial == "Diaspore Bank" , ]))
b_liver_lm_letters <- data.frame(Trial = "Diaspore Bank",
                                  TaxonomicMorphologicalGroup = "Liver.",
                                  trt = 3.5,
                                  Cover = 10,
                                  text = "NS")
anova(v_liver_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                       data = temp[temp$Trial == "Vegetation" , ]))
v_liver_lm_letters <- data.frame(Trial = "Vegetation",
                                 TaxonomicMorphologicalGroup = "Liver.",
                                 trt = 3.5,
                                 Cover = 10,
                                 text = "NS")

temp <- taxon.groups[taxon.groups$Group == "Other" , ]
anova(b_other_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                       data = temp[temp$Trial == "Diaspore Bank" , ]))
b_other_lm_letters <- data.frame(Trial = "Diaspore Bank",
                                 TaxonomicMorphologicalGroup = "Other",
                                 trt = 3.5,
                                 Cover = 3,
                                 text = "NS")
anova(v_other_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                       data = temp[temp$Trial == "Vegetation" , ]))
v_other_lm_letters <- data.frame(Trial = "Vegetation",
                                 TaxonomicMorphologicalGroup = "Other",
                                 trt = 3.5,
                                 Cover = 3,
                                 text = "F:N")

temp <- taxon.groups[taxon.groups$Group == "Pleur" , ]
anova(b_pleur_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                       data = temp[temp$Trial == "Diaspore Bank" , ]))
b_pleur_lm_letters <- data.frame(Trial = "Diaspore Bank",
                                 TaxonomicMorphologicalGroup = "Pleur.",
                                 trt = 3.5,
                                 Cover = 30,
                                 text = "NS")
anova(v_pleur_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                       data = temp[temp$Trial == "Vegetation" , ]))
v_pleur_lm_letters <- data.frame(Trial = "Vegetation",
                                 TaxonomicMorphologicalGroup = "Pleur.",
                                 trt = 3.5,
                                 Cover = 30,
                                 text = "N,F:N")

temp <- taxon.groups[taxon.groups$Group == "Pohlia" , ]
anova(b_pohlia_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                       data = temp[temp$Trial == "Diaspore Bank" , ]))
b_pohlia_lm_letters <- data.frame(Trial = "Diaspore Bank",
                                 TaxonomicMorphologicalGroup = "Pohlia",
                                 trt = 3.5,
                                 Cover = 60,
                                 text = "NS")
anova(v_pohlia_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                       data = temp[temp$Trial == "Vegetation" , ]))
v_pohlia_lm_letters <- data.frame(Trial = "Vegetation",
                                 TaxonomicMorphologicalGroup = "Pohlia",
                                 trt = 3.5,
                                 Cover = 60,
                                 text = "F")

temp <- taxon.groups[taxon.groups$Group == "Poly" , ]
anova(b_poly_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                        data = temp[temp$Trial == "Diaspore Bank" , ]))
b_poly_lm_letters <- data.frame(Trial = "Diaspore Bank",
                                  TaxonomicMorphologicalGroup = "Poly.",
                                  trt = 3.5,
                                  Cover = 6,
                                  text = "NS")
anova(v_poly_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                       data = temp[temp$Trial == "Vegetation" , ]))
v_poly_lm_letters <- data.frame(Trial = "Vegetation",
                                  TaxonomicMorphologicalGroup = "Poly.",
                                  trt = 3.5,
                                  Cover = 6,
                                  text = "N")

temp <- taxon.groups[taxon.groups$Group == "Ptych" , ]
anova(b_ptych_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                      data = temp[temp$Trial == "Diaspore Bank" , ]))
b_ptych_lm_letters <- data.frame(Trial = "Diaspore Bank",
                                TaxonomicMorphologicalGroup = "Ptych.",
                                trt = 3.5,
                                Cover = 30,
                                text = "NS")
anova(v_ptych_lm <- lm(asin(sqrt(Cover/100)) ~ Block + Fence * NPK,
                       data = temp[temp$Trial == "Vegetation" , ]))
v_ptych_lm_letters <- data.frame(Trial = "Vegetation",
                                TaxonomicMorphologicalGroup = "Ptych.",
                                trt = 3.5,
                                Cover = 30,
                                text = "F")

group.letters <- rbind(b_dicran_lm_letters, v_dicran_lm_letters,
                       b_liver_lm_letters, v_liver_lm_letters,
                       b_other_lm_letters, v_other_lm_letters,
                       b_pleur_lm_letters, v_pleur_lm_letters,
                       b_pohlia_lm_letters, v_pohlia_lm_letters,
                       b_poly_lm_letters, v_poly_lm_letters,
                       b_ptych_lm_letters, v_ptych_lm_letters)


taxon.groups.mean <- taxon.groups %>%
  group_by(Trial, NPK, Fence, trt, TaxonomicMorphologicalGroup) %>%
  summarize(Cover = mean(Cover), .groups = "keep")

ggplot(data = taxon.groups, aes(x = trt, y = Cover)) +
  geom_point(data = taxon.groups.mean, shape = "-", col = "red", size = 10) +
  geom_point() +
  geom_text(data = group.letters, aes(label = text), size = 3, hjust = "center") +
  facet_grid(rows = vars(factor(TaxonomicMorphologicalGroup, 
                                levels = c("Pohlia", "Poly.", "Ptych.",
                                           "Dicran.", "Pleur.", "Liver.", "Other"))),
             cols = vars(Trial), scales = "free_y") +
  labs(x = "Treatment", y = "Cover (%)") +
  theme_bw()
ggsave("graphs/Cover.TaxonGroups.png", width = 6.5, height = 6, units = "in", dpi = 600)
