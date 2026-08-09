# E3-ome gnomAD v4.1.1 Constraint Analysis

This folder contains a static GitHub Pages analysis portal for population genetic constraint across the E3-ome.

Run the reproducible build from the repository root:

```sh
Rscript Genetics/gnomAD/scripts/build_gnomad_constraint_site.R
```

The script downloads or reads:

- Feltham Laboratory E3-ome gene list
- gnomAD v4.1.1 constraint metrics `gnomad.v4.1.1.constraint_metrics.tsv.bgz`

Generated outputs include joined CSV/Excel tables, match-quality reports, summary statistics, plots, and the static website at `Genetics/gnomAD/index.html`.

The intended public URL is:

https://felthamlaboratory.github.io/E3-ome/Genetics/gnomAD/

Important interpretation limit: gnomAD constraint reflects depletion of population variation. It does not by itself prove cellular essentiality, disease causality, experimental functional importance, or therapeutic suitability.
