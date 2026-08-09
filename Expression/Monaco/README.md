# Immune E3-ome Atlas

Static atlas of the curated human E3-ome across Monaco immune-cell nTPM expression profiles.

## Files

- `index.html` - public-facing atlas website
- `data.js` - embedded atlas data generated from the source tables
- `e3ome_enrichment_scores.csv` - ranked E3 enrichment scores

## Hosting

Upload these files to the root of a GitHub repository and enable GitHub Pages from the `main` branch. The site does not require a backend or build step.

## Methods note

Scores describe relative cell-type-biased expression across immune populations using summarized nTPM values. They should not be interpreted as replicate-level differential expression.
