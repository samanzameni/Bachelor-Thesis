# Figures (base R graphics, saved as PNG) ------------------------------------

save_png <- function(file, expr, width = 1600, height = 1000, res = 150) {
  png(file, width = width, height = height, res = res)
  on.exit(dev.off())
  force(expr)
}

#' Dendrogram with the partition chosen by `k` drawn as boxes and the Mojena
#' thresholds for several alphas drawn as dashed lines.
plot_dendrogram <- function(hc, ds_name, linkage, k, alphas, file) {
  save_png(file, {
    plot(hc, labels = FALSE, hang = -1, xlab = "", sub = "",
         main = sprintf("%s - %s linkage (boxes: silhouette choice, k = %d)",
                        ds_name, linkage, k))
    cols <- grDevices::hcl.colors(length(alphas), "Dark 3")
    for (i in seq_along(alphas)) {
      abline(h = mojena_threshold(hc, alphas[i]), lty = 2, col = cols[i])
    }
    if (k >= 2) rect.hclust(hc, k = k, border = "grey30")
    legend("topright", lty = 2, col = cols, bty = "n",
           legend = sprintf("Mojena alpha = %.2f", alphas))
  })
}

#' Fusion heights of the last merges, with Mojena thresholds for several alphas.
plot_fusion_heights <- function(hc, ds_name, linkage, alphas, file, last = 30) {
  h <- hc$height
  n <- length(h) + 1
  idx <- seq(length(h) - last + 1, length(h))
  clusters_after <- n - idx
  thresholds <- vapply(alphas, function(a) mojena_threshold(hc, a), numeric(1))
  save_png(file, {
    plot(clusters_after, h[idx], type = "b", pch = 19,
         ylim = range(c(h[idx], thresholds)),
         xlab = "Number of clusters after merge", ylab = "Fusion height",
         main = sprintf("%s - %s linkage: fusion heights", ds_name, linkage))
    cols <- grDevices::hcl.colors(length(alphas), "Dark 3")
    abline(h = thresholds, lty = 2, col = cols)
    legend("topright", lty = 2, col = cols, bty = "n",
           legend = sprintf("Mojena alpha = %.2f", alphas))
  })
}

#' Average silhouette width against k, one line per linkage.
plot_silhouette <- function(sil, ds_name, file) {
  wide <- reshape(sil[, c("linkage", "k", "avg_silhouette")],
                  idvar = "k", timevar = "linkage", direction = "wide")
  linkages <- sub("^avg_silhouette\\.", "", names(wide)[-1])
  save_png(file, {
    matplot(wide$k, as.matrix(wide[, -1]), type = "b", pch = 19, lty = 1,
            col = seq_along(linkages), xlab = "Number of clusters k",
            ylab = "Average silhouette width",
            main = sprintf("%s: average silhouette width", ds_name))
    legend("topright", legend = linkages, col = seq_along(linkages),
           lty = 1, pch = 19, bty = "n")
  })
}
