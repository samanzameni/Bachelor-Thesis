# Stopping rules for choosing the number of clusters ------------------------
#
# All rules take an `hclust` object built with a monotone linkage (single,
# complete, average or Ward), so that the fusion heights `hc$height` are
# non-decreasing in merge order. Merge i (i = 1, ..., n - 1) reduces the number
# of clusters from n - i + 1 to n - i.

#' Mojena's upper-tail rule (Mojena, 1977).
#'
#' Stop at the first merge whose fusion height exceeds
#'   mean(h) + alpha * sd(h),
#' i.e. keep the partition that exists just before that merge.
#' alpha = 1.25 was the best constant in Milligan and Cooper's (1985, p. 164)
#' simulation; Mojena (1977) himself recommended 2.75-3.50.
#'
#' @return number of clusters (integer)
mojena_k <- function(hc, alpha = 1.25) {
  h <- hc$height
  n <- length(h) + 1
  first_large <- which(h > mojena_threshold(hc, alpha))[1]
  if (is.na(first_large)) return(1L)
  as.integer(n - first_large + 1)
}

mojena_threshold <- function(hc, alpha = 1.25) {
  mean(hc$height) + alpha * sd(hc$height)
}

#' Largest-jump ("elbow") rule: cut where the increase in fusion height between
#' two consecutive merges is largest, restricted to k in k_range.
max_jump_k <- function(hc, k_range = 2:10) {
  h <- hc$height
  n <- length(h) + 1
  # Cutting at k clusters means stopping before merge n - k + 1, whose height
  # jump over the previous merge is h[n - k + 1] - h[n - k].
  jumps <- vapply(k_range, function(k) h[n - k + 1] - h[n - k], numeric(1))
  as.integer(k_range[which.max(jumps)])
}

#' Average silhouette width of the partition at each k.
silhouette_profile <- function(hc, d, k_range = 2:10) {
  vapply(k_range, function(k) {
    s <- cluster::silhouette(cutree(hc, k = k), d)
    mean(s[, "sil_width"])
  }, numeric(1))
}

silhouette_k <- function(hc, d, k_range = 2:10) {
  as.integer(k_range[which.max(silhouette_profile(hc, d, k_range))])
}

#' Majority vote over several NbClust indices (Euclidean data only).
#'
#' Indices that fail on a given data set (e.g. because of collinear features)
#' are skipped. Returns NA if no index succeeds.
nbclust_vote <- function(x, linkage, k_range = 2:10,
                         indices = c("ch", "db", "silhouette", "hartigan",
                                     "duda", "mcclain")) {
  votes <- vapply(indices, function(idx) {
    tryCatch({
      res <- NULL
      # NbClust prints progress text; keep the console clean.
      utils::capture.output(res <- suppressWarnings(NbClust::NbClust(
        data = x, distance = "euclidean",
        min.nc = min(k_range), max.nc = max(k_range),
        method = linkage, index = idx
      )))
      as.numeric(res$Best.nc[1])
    }, error = function(e) NA_real_)
  }, numeric(1))
  votes <- votes[!is.na(votes)]
  if (length(votes) == 0) return(NA_integer_)
  counts <- table(votes)
  as.integer(names(counts)[which.max(counts)])
}
