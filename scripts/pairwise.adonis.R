pairwise.adonis <- function(resp, fact, p.method = "none", nperm = 999) {
  require(vegan)
  resp <- as.matrix(resp)
  fact <- factor(fact)
  fun.p <- function(i, j) {
    fact2 <- droplevels(fact[as.numeric(fact) %in% c(i, j)])
    index <- which(fact %in% levels(fact2))
    resp2 <- as.dist(resp[index, index])
    result <- adonis(resp2 ~ fact2, permutations = nperm)
    result$aov.tab[1, "Pr(>F)"]
  }
  multcomp <- pairwise.table(fun.p, levels(fact), p.adjust.method = p.method)
  return(list(fact = levels(fact), p.value = multcomp, p.adjust.method = p.method))
}
