# Determining the number of clusters in hierarchical clustering
# ------------------------------------------------------------------------
# Runs every linkage and stopping rule on both data sets, evaluates the
# chosen partitions against the known class labels, and writes tables to
# results/ and figures to figures/.
#
# Usage (from the repository root):   Rscript run_all.R

required <- c("cluster", "mclust", "NbClust")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       ". Run source(\"install_packages.R\") first.")
}

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

set.seed(42)
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

linkages      <- c("single", "complete", "average", "ward.D2")
mojena_alphas <- c(1.25, 2.00, 2.75, 3.50)
k_range       <- 2:10

datasets <- list(load_wdbc(), load_heart())

all_selection <- list(); all_linkages <- list(); all_baselines <- list()

for (ds in datasets) {
  message("Analysing ", ds$name, " (n = ", attr(ds$dist, "Size"), ")")
  res <- analyse_dataset(ds, linkages, mojena_alphas, k_range)

  all_selection[[ds$name]] <- res$selection
  all_linkages[[ds$name]]  <- res$linkages
  all_baselines[[ds$name]] <- baseline_ari(ds)

  plot_silhouette(res$silhouette, ds$name,
                  file.path("figures", sprintf("%s_silhouette.png", ds$name)))
  for (linkage in linkages) {
    hc <- res$trees[[linkage]]
    k_sil <- silhouette_k(hc, ds$dist, k_range)
    plot_dendrogram(hc, ds$name, linkage, k_sil, mojena_alphas,
                    file.path("figures", sprintf("%s_%s_dendrogram.png", ds$name, linkage)))
    plot_fusion_heights(hc, ds$name, linkage, mojena_alphas,
                        file.path("figures", sprintf("%s_%s_heights.png", ds$name, linkage)))
  }
}

selection <- do.call(rbind, all_selection)
linkage_summary <- do.call(rbind, all_linkages)
baselines <- do.call(rbind, all_baselines)

write.csv(selection, file.path("results", "k_selection.csv"), row.names = FALSE)
write.csv(linkage_summary, file.path("results", "cophenetic_correlation.csv"), row.names = FALSE)
write.csv(baselines, file.path("results", "baselines.csv"), row.names = FALSE)

# Compact console summary: chosen k (ARI) for every linkage x rule.
selection$cell <- ifelse(is.na(selection$ari), sprintf("%s (-)", selection$k),
                         sprintf("%d (%.2f)", selection$k, selection$ari))
for (name in unique(selection$dataset)) {
  rows <- selection[selection$dataset == name, c("rule", "linkage", "cell")]
  tab <- reshape(rows, idvar = "rule", timevar = "linkage", direction = "wide")
  names(tab) <- sub("^cell\\.", "", names(tab))
  cat("\n==", name, ": chosen k (ARI vs. true labels) ==\n")
  print(tab, row.names = FALSE)
}
cat("\n== Cophenetic correlation ==\n")
print(linkage_summary, row.names = FALSE, digits = 3)
cat("\n== Non-hierarchical baselines at the true k ==\n")
print(baselines, row.names = FALSE, digits = 3)
