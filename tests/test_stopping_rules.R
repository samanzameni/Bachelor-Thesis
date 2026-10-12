# Sanity checks on synthetic data with a known number of clusters.
# Usage (from the repository root):   Rscript tests/test_stopping_rules.R

source(file.path("R", "stopping_rules.R"))

set.seed(1)
blob <- function(n, cx, cy) cbind(rnorm(n, cx, 0.2), rnorm(n, cy, 0.2))
x <- rbind(blob(50, 0, 0), blob(50, 10, 0), blob(50, 0, 10))
d <- dist(x)

for (linkage in c("complete", "average", "ward.D2")) {
  hc <- hclust(d, method = linkage)
  stopifnot(
    mojena_k(hc, 1.25) == 3,
    max_jump_k(hc) == 3,
    silhouette_k(hc, d) == 3
  )
}

# Index bookkeeping: on 4 points the merges give 3, 2, 1 clusters.
hc_small <- hclust(dist(c(0, 1, 10, 11)), method = "single")
stopifnot(max_jump_k(hc_small, k_range = 2:3) == 2)

cat("All stopping-rule tests passed.\n")
