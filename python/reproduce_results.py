"""Python reproduction of the analysis in run_all.R.

Implements the same linkages, stopping rules and evaluation with SciPy and
scikit-learn, and writes the tables and figures reported in the README.
(The NbClust majority vote has no direct Python equivalent and is omitted.)

Usage (from the repository root):
    pip install numpy pandas scipy scikit-learn matplotlib
    python python/reproduce_results.py
"""

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.cluster.hierarchy import cophenet, dendrogram, fcluster, linkage
from scipy.spatial.distance import pdist, squareform
from sklearn.cluster import KMeans
from sklearn.metrics import adjusted_rand_score, silhouette_score

ROOT = Path(__file__).resolve().parents[1]
RESULTS = ROOT / "results"
FIGURES = ROOT / "figures"

LINKAGES = {"single": "single", "complete": "complete",
            "average": "average", "ward.D2": "ward"}  # R name -> SciPy name
MOJENA_ALPHAS = (1.25, 2.0, 2.75, 3.5)
K_RANGE = range(2, 11)
SEED = 42


# --- data -------------------------------------------------------------------

def load_wdbc():
    df = pd.read_csv(ROOT / "data" / "wdbc.csv")
    labels = (df["diagnosis"] == "M").astype(int).to_numpy()
    x = df.drop(columns="diagnosis").to_numpy(dtype=float)
    x = (x - x.mean(axis=0)) / x.std(axis=0, ddof=1)  # same as R scale()
    return {"name": "wdbc", "x": x, "labels": labels,
            "dist": squareform(pdist(x)), "euclidean": True}


def gower(df, categorical):
    """Gower dissimilarity (as cluster::daisy): range-scaled absolute
    differences for numeric columns, simple mismatch for categorical ones,
    averaged over the columns observed in both rows."""
    n = len(df)
    total = np.zeros((n, n))
    weight = np.zeros((n, n))
    for col in df.columns:
        v = df[col].to_numpy(dtype=float)
        both = np.outer(~np.isnan(v), ~np.isnan(v))
        if col in categorical:
            d = (v[:, None] != v[None, :]).astype(float)
        else:
            d = np.abs(v[:, None] - v[None, :]) / (np.nanmax(v) - np.nanmin(v))
        d[~both] = 0.0
        total += d
        weight += both
    g = total / weight
    np.fill_diagonal(g, 0.0)
    return g


def load_heart():
    df = pd.read_csv(ROOT / "data" / "heart_cleveland.csv")
    labels = (df["num"] > 0).astype(int).to_numpy()
    x = df.drop(columns="num")
    categorical = {"sex", "cp", "fbs", "restecg", "exang", "slope", "thal"}
    return {"name": "heart", "x": x, "labels": labels,
            "dist": gower(x, categorical), "euclidean": False}


# --- stopping rules (see R/stopping_rules.R) ---------------------------------

def mojena_threshold(h, alpha):
    return h.mean() + alpha * h.std(ddof=1)


def mojena_k(h, alpha=1.25):
    n = len(h) + 1
    large = np.nonzero(h > mojena_threshold(h, alpha))[0]
    return 1 if len(large) == 0 else n - (large[0] + 1) + 1


def max_jump_k(h, k_range=K_RANGE):
    n = len(h) + 1
    jumps = [h[n - k] - h[n - k - 1] for k in k_range]  # 0-based heights
    return list(k_range)[int(np.argmax(jumps))]


def silhouette_profile(z, d, k_range=K_RANGE):
    return [silhouette_score(d, fcluster(z, k, "maxclust"), metric="precomputed")
            for k in k_range]


# --- analysis ----------------------------------------------------------------

def analyse(ds):
    condensed = squareform(ds["dist"], checks=False)
    selection, linkage_rows, silhouettes, trees = [], [], {}, {}
    for r_name, scipy_name in LINKAGES.items():
        z = linkage(condensed, scipy_name)
        h = z[:, 2]
        trees[r_name] = z
        sil = silhouette_profile(z, ds["dist"])
        silhouettes[r_name] = sil
        rules = {f"mojena_{a:.2f}": mojena_k(h, a) for a in MOJENA_ALPHAS}
        rules["max_jump"] = max_jump_k(h)
        rules["silhouette"] = list(K_RANGE)[int(np.argmax(sil))]
        rules["true_k"] = 2
        for rule, k in rules.items():
            ari = (adjusted_rand_score(ds["labels"], fcluster(z, k, "maxclust"))
                   if k >= 2 else np.nan)
            selection.append({"dataset": ds["name"], "linkage": r_name,
                              "rule": rule, "k": k, "ari": ari})
        linkage_rows.append({"dataset": ds["name"], "linkage": r_name,
                             "cophenetic_correlation": np.corrcoef(cophenet(z), condensed)[0, 1]})
    return pd.DataFrame(selection), pd.DataFrame(linkage_rows), silhouettes, trees


def baseline(ds):
    if ds["euclidean"]:
        clusters = KMeans(n_clusters=2, n_init=25, random_state=SEED).fit_predict(ds["x"])
        method = "kmeans"
    else:
        clusters = pam(ds["dist"], k=2)
        method = "pam"
    return {"dataset": ds["name"], "method": method, "k": 2,
            "ari": adjusted_rand_score(ds["labels"], clusters)}


def pam(d, k, max_iter=100):
    """Minimal PAM (k-medoids) with BUILD initialisation and SWAP refinement."""
    n = len(d)
    medoids = [int(np.argmin(d.sum(axis=1)))]
    while len(medoids) < k:
        nearest = d[:, medoids].min(axis=1)
        gains = [np.maximum(nearest - d[:, c], 0).sum() if c not in medoids else -1
                 for c in range(n)]
        medoids.append(int(np.argmax(gains)))
    cost = d[:, medoids].min(axis=1).sum()
    for _ in range(max_iter):
        best = (cost, None, None)
        for i in range(k):
            for c in range(n):
                if c in medoids:
                    continue
                trial = medoids.copy()
                trial[i] = c
                trial_cost = d[:, trial].min(axis=1).sum()
                if trial_cost < best[0] - 1e-12:
                    best = (trial_cost, i, c)
        if best[1] is None:
            break
        cost = best[0]
        medoids[best[1]] = best[2]
    return d[:, medoids].argmin(axis=1)


# --- figures -----------------------------------------------------------------

def plot_silhouette(silhouettes, name):
    fig, ax = plt.subplots(figsize=(8, 5))
    for linkage_name, values in silhouettes.items():
        ax.plot(list(K_RANGE), values, marker="o", label=linkage_name)
    ax.set_xlabel("Number of clusters k")
    ax.set_ylabel("Average silhouette width")
    ax.set_title(f"{name}: average silhouette width")
    ax.legend(frameon=False)
    fig.tight_layout()
    fig.savefig(FIGURES / f"{name}_silhouette.png", dpi=150)
    plt.close(fig)


def plot_ward_dendrogram(z, name):
    h = z[:, 2]
    fig, ax = plt.subplots(figsize=(10, 5))
    dendrogram(z, no_labels=True, color_threshold=(h[-2] + h[-1]) / 2,
               above_threshold_color="grey", ax=ax)
    colours = plt.cm.viridis(np.linspace(0.1, 0.9, len(MOJENA_ALPHAS)))
    for alpha, colour in zip(MOJENA_ALPHAS, colours):
        ax.axhline(mojena_threshold(h, alpha), ls="--", lw=1, color=colour,
                   label=f"Mojena α = {alpha:.2f} (k = {mojena_k(h, alpha)})")
    ax.set_ylabel("Fusion height")
    ax.set_title(f"{name}: Ward linkage dendrogram (colours: k = 2)")
    ax.legend(frameon=False, loc="upper right")
    fig.tight_layout()
    fig.savefig(FIGURES / f"{name}_ward_dendrogram.png", dpi=150)
    plt.close(fig)


def main():
    RESULTS.mkdir(exist_ok=True)
    FIGURES.mkdir(exist_ok=True)
    selections, linkages, baselines = [], [], []
    for ds in (load_wdbc(), load_heart()):
        sel, link, sil, trees = analyse(ds)
        selections.append(sel)
        linkages.append(link)
        baselines.append(baseline(ds))
        plot_silhouette(sil, ds["name"])
        plot_ward_dendrogram(trees["ward.D2"], ds["name"])

    selection = pd.concat(selections, ignore_index=True)
    linkage_summary = pd.concat(linkages, ignore_index=True)
    baseline_table = pd.DataFrame(baselines)
    selection.to_csv(RESULTS / "k_selection.csv", index=False)
    linkage_summary.to_csv(RESULTS / "cophenetic_correlation.csv", index=False)
    baseline_table.to_csv(RESULTS / "baselines.csv", index=False)

    pd.set_option("display.width", 120)
    for name, sub in selection.groupby("dataset", sort=False):
        cells = sub.assign(cell=[f"{k} ({a:.2f})" if not np.isnan(a) else f"{k} (-)"
                                 for k, a in zip(sub["k"], sub["ari"])])
        table = cells.pivot(index="rule", columns="linkage", values="cell")
        print(f"\n== {name}: chosen k (ARI vs. true labels) ==")
        print(table.loc[sub["rule"].unique(), list(LINKAGES)])
    print("\n== Cophenetic correlation ==")
    print(linkage_summary.round(3).to_string(index=False))
    print("\n== Non-hierarchical baselines at the true k ==")
    print(baseline_table.round(3).to_string(index=False))


if __name__ == "__main__":
    main()
