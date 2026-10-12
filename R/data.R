# Data loading -------------------------------------------------------------
#
# Each loader returns a list with:
#   name   - short dataset identifier
#   x      - the feature data used for clustering (labels removed)
#   labels - the known class labels, used ONLY for external evaluation
#   dist   - the dissimilarity matrix used by hierarchical clustering
#   euclidean - TRUE when `dist` is Euclidean on `x` (needed by NbClust)

#' Breast Cancer Wisconsin (Diagnostic) data set: 569 samples, 30 numeric
#' features, diagnosis B (benign) or M (malignant).
#'
#' The features have very different scales (e.g. area ~ 10^3, fractal
#' dimension ~ 10^-2), so they are standardised to zero mean and unit variance
#' before computing Euclidean distances.
load_wdbc <- function(path = file.path("data", "wdbc.csv")) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  labels <- factor(df$diagnosis, levels = c("B", "M"))
  x <- scale(as.matrix(df[, setdiff(names(df), "diagnosis")]))
  list(
    name = "wdbc",
    x = x,
    labels = labels,
    dist = dist(x, method = "euclidean"),
    euclidean = TRUE
  )
}

#' Cleveland Heart Disease data set: 303 patients, 13 attributes of mixed
#' type. The original `num` column (0 = no disease, 1-4 = disease) is the
#' diagnosis; it is removed from the features and binarised for evaluation.
#'
#' Because several attributes are categorical (chest pain type, ECG result,
#' slope, thalassemia, ...), Euclidean distance on their numeric codes is not
#' meaningful. Gower's dissimilarity handles mixed numeric/categorical data and
#' missing values (one missing `thal` value).
load_heart <- function(path = file.path("data", "heart_cleveland.csv")) {
  df <- read.csv(path, na.strings = "NA")
  labels <- factor(ifelse(df$num > 0, "disease", "no_disease"),
                   levels = c("no_disease", "disease"))
  x <- df[, setdiff(names(df), "num")]
  categorical <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "thal")
  for (col in categorical) x[[col]] <- factor(x[[col]])
  list(
    name = "heart",
    x = x,
    labels = labels,
    dist = cluster::daisy(x, metric = "gower"),
    euclidean = FALSE
  )
}
