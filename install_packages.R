# Install the CRAN packages used by this project (run once).
required <- c("cluster", "mclust", "NbClust")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)
