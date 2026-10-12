# Analysis and evaluation ----------------------------------------------------

#' Cophenetic correlation: how faithfully the dendrogram preserves the original
#' pairwise dissimilarities (closer to 1 is better).
cophenetic_correlation <- function(hc, d) {
  cor(as.vector(cophenetic(hc)), as.vector(d))
}

#' Adjusted Rand Index between a partition and the known labels
#' (1 = perfect agreement, ~0 = chance level).
ari <- function(clusters, labels) {
  mclust::adjustedRandIndex(clusters, labels)
}

#' Run every linkage and stopping rule on one data set.
#'
#' @return list with
#'   $selection  one row per (linkage, rule): chosen k and its ARI
#'   $linkages   one row per linkage: cophenetic correlation
#'   $silhouette average silhouette width for each linkage and k
#'   $trees      the fitted hclust objects, by linkage
analyse_dataset <- function(ds, linkages, mojena_alphas, k_range = 2:10) {
  n_classes <- nlevels(ds$labels)
  selection <- list()
  linkage_rows <- list()
  silhouette_rows <- list()
  trees <- list()

  for (linkage in linkages) {
    hc <- hclust(ds$dist, method = linkage)
    trees[[linkage]] <- hc

    rules <- c(
      setNames(vapply(mojena_alphas, function(a) mojena_k(hc, a), integer(1)),
               sprintf("mojena_%.2f", mojena_alphas)),
      max_jump = max_jump_k(hc, k_range),
      silhouette = silhouette_k(hc, ds$dist, k_range)
    )
    if (ds$euclidean) {
      rules <- c(rules, nbclust_vote = nbclust_vote(ds$x, linkage, k_range))
    }
    # Reference row: the partition with the true number of classes.
    rules <- c(rules, true_k = n_classes)

    for (rule in names(rules)) {
      k <- rules[[rule]]
      selection[[length(selection) + 1]] <- data.frame(
        dataset = ds$name, linkage = linkage, rule = rule, k = k,
        ari = if (is.na(k) || k < 2) NA_real_ else
          ari(cutree(hc, k = k), ds$labels)
      )
    }

    linkage_rows[[linkage]] <- data.frame(
      dataset = ds$name, linkage = linkage,
      cophenetic_correlation = cophenetic_correlation(hc, ds$dist)
    )
    silhouette_rows[[linkage]] <- data.frame(
      dataset = ds$name, linkage = linkage, k = k_range,
      avg_silhouette = silhouette_profile(hc, ds$dist, k_range)
    )
  }

  list(
    selection = do.call(rbind, selection),
    linkages = do.call(rbind, linkage_rows),
    silhouette = do.call(rbind, silhouette_rows),
    trees = trees
  )
}

#' Non-hierarchical baseline at the true number of classes:
#' k-means for Euclidean data, PAM (k-medoids) for Gower dissimilarities.
baseline_ari <- function(ds, nstart = 25) {
  k <- nlevels(ds$labels)
  if (ds$euclidean) {
    clusters <- kmeans(ds$x, centers = k, nstart = nstart)$cluster
    method <- "kmeans"
  } else {
    clusters <- cluster::pam(ds$dist, k = k, diss = TRUE)$clustering
    method <- "pam"
  }
  data.frame(dataset = ds$name, method = method, k = k,
             ari = ari(clusters, ds$labels))
}
