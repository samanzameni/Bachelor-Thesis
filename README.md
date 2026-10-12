# Determining the Number of Clusters in Hierarchical Clustering

**B.Sc. thesis, Amirkabir University of Technology (2015)** · Saman Zameni · Supervisor: Prof. Adel Mohammadpour

Hierarchical clustering produces a full dendrogram, but most applications need a single partition, so the analyst has to decide where to cut the tree. This project compares rules for choosing the number of clusters *k*, focusing on **Mojena's stopping rule** (Mojena, 1977), on two real data sets with known class labels. Because the true classes are known, every chosen partition can be scored objectively.

> The code in this repository is a 2026 refactoring of the original thesis scripts (which remain in the git history). It fixes several issues in those scripts, including unscaled features, the class label being used as a clustering feature, and hard-coded file paths. It also adds an external evaluation that the original analysis did not have.

## Method

**Data sets**

| Data set | n | Features | Distance | Known classes |
|---|---|---|---|---|
| Breast Cancer Wisconsin (Diagnostic) | 569 | 30 numeric | Euclidean on standardised features | benign / malignant |
| Cleveland Heart Disease | 303 | 13 mixed (6 numeric, 7 categorical) | Gower dissimilarity | disease / no disease |

The class labels are never used for clustering; they are used only to evaluate the chosen partitions.

**Linkages:** single, complete, average and Ward (`ward.D2`). The centroid and median linkages are excluded because they can produce non-monotone dendrograms, which makes height-based stopping rules undefined.

**Rules for choosing *k*:**

- **Mojena's upper-tail rule:** stop at the first merge whose fusion height exceeds mean(h) + α·sd(h). Reported for α = 1.25 (Milligan & Cooper, 1985), 2.0, 2.75 and 3.5.
- **Largest jump:** cut where the increase in fusion height between consecutive merges is largest (*k* = 2–10).
- **Average silhouette width:** the *k* in 2–10 with the highest average silhouette (Rousseeuw, 1987).
- **NbClust majority vote:** majority vote over six indices (CH, DB, silhouette, Hartigan, Duda–Hart, McClain). Euclidean data only.

**Evaluation**

- **Adjusted Rand Index (ARI)** between each chosen partition and the true classes. ARI is 1 for perfect agreement and about 0 at chance level.
- **Cophenetic correlation** for each linkage.
- **Non-hierarchical baselines** at the true *k*: k-means (25 random starts) and PAM (k-medoids) on the Gower dissimilarity.

## Results

*To be added after the first full run of `run_all.R`.*

## Repository structure

```
run_all.R                  main script: runs everything, writes results/ and figures/
install_packages.R         installs the required CRAN packages
R/data.R                   data loading, scaling and dissimilarities
R/stopping_rules.R         Mojena, largest-jump, silhouette and NbClust rules
R/evaluation.R             cophenetic correlation, ARI, baselines
R/plots.R                  dendrograms, fusion-height and silhouette plots
tests/test_stopping_rules.R  checks the rules on synthetic data with 3 known clusters
data/                      the two data sets as labelled CSV files
```

## How to run

Requires R ≥ 4.0. From the repository root:

```r
source("install_packages.R")   # cluster, mclust, NbClust (first time only)
source("tests/test_stopping_rules.R")
source("run_all.R")
```

or from a terminal: `Rscript run_all.R`. Results are written to `results/` (CSV tables) and `figures/` (PNG plots). The random seed is fixed (`set.seed(42)`), so the k-means baseline is reproducible.

## Data notes

- **Breast Cancer Wisconsin (Diagnostic):** Wolberg, Street & Mangasarian, UCI Machine Learning Repository. The diagnosis column was restored by matching the feature rows to the UCI release.
- **Cleveland Heart Disease:** Detrano et al., UCI Machine Learning Repository. The original `num` attribute (0 = no disease, 1–4 = disease) is binarised. In this copy of the file the missing values of `ca` had already been filled in, and one missing `thal` value is recorded as `NA`; Gower's dissimilarity handles it.
- Ward linkage assumes Euclidean distances. Its results on the Gower dissimilarity (heart data) are reported for comparison, but they should be read with this caveat.

## References

- Mojena, R. (1977). Hierarchical grouping methods and stopping rules: an evaluation. *The Computer Journal*, 20(4), 359–363.
- Milligan, G. W., & Cooper, M. C. (1985). An examination of procedures for determining the number of clusters in a data set. *Psychometrika*, 50(2), 159–179.
- Rousseeuw, P. J. (1987). Silhouettes: a graphical aid to the interpretation and validation of cluster analysis. *Journal of Computational and Applied Mathematics*, 20, 53–65.
- Gower, J. C. (1971). A general coefficient of similarity and some of its properties. *Biometrics*, 27(4), 857–871.
- Hubert, L., & Arabie, P. (1985). Comparing partitions. *Journal of Classification*, 2(1), 193–218.
- Charrad, M., Ghazzali, N., Boiteau, V., & Niknafs, A. (2014). NbClust: an R package for determining the relevant number of clusters in a data set. *Journal of Statistical Software*, 61(6), 1–36.
