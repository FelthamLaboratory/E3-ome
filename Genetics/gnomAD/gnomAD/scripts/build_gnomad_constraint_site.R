#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else if (!is.null(sys.frames()[[1]]$ofile)) {
  script_path <- normalizePath(sys.frames()[[1]]$ofile, mustWork = TRUE)
} else {
  script_path <- normalizePath("Genetics/gnomAD/scripts/build_gnomad_constraint_site.R", mustWork = TRUE)
}
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
repo_root <- normalizePath(file.path(root, "..", ".."), mustWork = TRUE)
dir.create(file.path(root, "data", "raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "plots"), recursive = TRUE, showWarnings = FALSE)

e3_url <- "https://github.com/FelthamLaboratory/E3-ome/raw/refs/heads/main/data/High-confidence%2020260809.xlsx"
gnomad_url <- "https://storage.googleapis.com/gcp-public-data--gnomad/release/4.1.1/constraint/gnomad.v4.1.1.constraint_metrics.tsv.bgz"
e3_local <- file.path(repo_root, "High-confidence 20260809.xlsx")
e3_raw <- file.path(root, "data", "raw", "High-confidence 20260809.xlsx")
gnomad_raw <- file.path(root, "data", "raw", "gnomad.v4.1.1.constraint_metrics.tsv.bgz")

if (!file.exists(e3_raw)) {
  if (file.exists(e3_local)) {
    file.copy(e3_local, e3_raw, overwrite = TRUE)
  } else {
    download.file(e3_url, e3_raw, mode = "wb", quiet = FALSE)
  }
}

if (!file.exists(gnomad_raw)) {
  download.file(gnomad_url, gnomad_raw, mode = "wb", quiet = FALSE)
}

clean_id <- function(x) sub("\\.[0-9]+$", "", as.character(x))
is_true <- function(x) tolower(as.character(x)) %in% c("true", "1", "yes")
empty_flag <- function(x) is.na(x) | x == "" | x == "[]" | x == "NA"
safe_num <- function(x) suppressWarnings(as.numeric(x))
qfmt <- function(x) ifelse(is.na(x), NA_character_, formatC(x, digits = 4, format = "fg", flag = "#"))

core_class_levels <- c("RING", "degenerate RING", "HECT", "RBR", "CRL1", "CRL2", "CRL3", "CRL4", "CRL5", "Atypical", "APC/C")

core_classes <- function(x) {
  z <- tolower(ifelse(is.na(x), "", x))
  out <- character(0)
  if (grepl("^ring(,|$)", z)) out <- c(out, "RING")
  if (grepl("degenerate ring|u-box|sp-ring", z)) out <- c(out, "degenerate RING")
  if (grepl("hect", z)) out <- c(out, "HECT")
  if (grepl("ring-between-ring", z)) out <- c(out, "RBR")
  if (grepl("crl1|f-box", z)) out <- c(out, "CRL1")
  if (grepl("crl2", z)) out <- c(out, "CRL2")
  if (grepl("crl3|btb", z)) out <- c(out, "CRL3")
  if (grepl("crl4|ddb1", z)) out <- c(out, "CRL4")
  if (grepl("crl5", z)) out <- c(out, "CRL5")
  if (grepl("atypical", z)) out <- c(out, "Atypical")
  if (grepl("apc", z)) out <- c(out, "APC/C")
  out <- unique(out[out %in% core_class_levels])
  if (!length(out)) out <- "Other/unknown"
  out[order(match(out, c(core_class_levels, "Other/unknown")))]
}

major_class <- function(x) {
  vapply(x, function(one) paste(core_classes(one), collapse = "; "), character(1))
}

expand_class_memberships <- function(df) {
  pieces <- strsplit(as.character(df$major_e3_class), "; ", fixed = TRUE)
  idx <- rep(seq_len(nrow(df)), lengths(pieces))
  out <- df[idx, , drop = FALSE]
  out$major_e3_class <- unlist(pieces, use.names = FALSE)
  out
}

wilson_ci <- function(k, n, conf = 0.95) {
  if (is.na(n) || n == 0) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  den <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / den
  half <- z * sqrt((p * (1 - p) / n + z^2 / (4 * n^2))) / den
  c(max(0, centre - half), min(1, centre + half))
}

write_simple_xlsx <- function(df, path, sheet_name = "E3 gnomAD constraint") {
  xml_escape <- function(x) {
    x <- ifelse(is.na(x), "", as.character(x))
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }
  col_ref <- function(n) {
    vapply(n, function(i) {
      s <- ""
      while (i > 0) {
        r <- (i - 1) %% 26
        s <- paste0(LETTERS[r + 1], s)
        i <- (i - r - 1) %/% 26
      }
      s
    }, character(1))
  }
  td <- tempfile("xlsx")
  dir.create(file.path(td, "_rels"), recursive = TRUE)
  dir.create(file.path(td, "xl", "_rels"), recursive = TRUE)
  dir.create(file.path(td, "xl", "worksheets"), recursive = TRUE)
  writeLines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/></Types>', file.path(td, "[Content_Types].xml"))
  writeLines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>', file.path(td, "_rels", ".rels"))
  wb <- sprintf('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="%s" sheetId="1" r:id="rId1"/></sheets></workbook>', xml_escape(sheet_name))
  writeLines(wb, file.path(td, "xl", "workbook.xml"))
  writeLines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/></Relationships>', file.path(td, "xl", "_rels", "workbook.xml.rels"))
  con <- file(file.path(td, "xl", "worksheets", "sheet1.xml"), open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>', con)
  header <- as.data.frame(as.list(stats::setNames(names(df), names(df))), stringsAsFactors = FALSE)
  all_rows <- rbind(header, as.data.frame(df, stringsAsFactors = FALSE))
  for (r in seq_len(nrow(all_rows))) {
    cells <- vapply(seq_along(all_rows), function(c) {
      val <- all_rows[[c]][r]
      ref <- paste0(col_ref(c), r)
      if (is.numeric(val) && !is.na(val)) {
        sprintf('<c r="%s"><v>%s</v></c>', ref, format(val, scientific = FALSE, trim = TRUE))
      } else {
        sprintf('<c r="%s" t="inlineStr"><is><t>%s</t></is></c>', ref, xml_escape(val))
      }
    }, character(1))
    writeLines(sprintf('<row r="%s">%s</row>', r, paste(cells, collapse = "")), con)
  }
  writeLines('</sheetData></worksheet>', con)
  close(con)
  on.exit(NULL, add = FALSE)
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)
  if (file.exists(path)) unlink(path)
  utils::zip(zipfile = path, files = c("[Content_Types].xml", "_rels/.rels", "xl/workbook.xml", "xl/_rels/workbook.xml.rels", "xl/worksheets/sheet1.xml"), flags = "-q")
  setwd(old)
}

message("Reading E3 workbook")
e3 <- read_excel(e3_raw, sheet = 1)
e3_columns <- names(e3)
required_e3 <- c("Gene", "GeneID", "UniProt accession", "Protein class")
missing_e3 <- setdiff(required_e3, e3_columns)
if (length(missing_e3)) stop("E3 workbook is missing required columns: ", paste(missing_e3, collapse = ", "))

e3 <- e3 %>%
  mutate(
    input_order = row_number(),
    e3_gene_symbol = as.character(Gene),
    e3_symbol_key = toupper(e3_gene_symbol),
    ncbi_gene_id = as.character(GeneID),
    uniprot_accession = as.character(`UniProt accession`),
    e3_protein_class = as.character(`Protein class`),
    major_e3_class = major_class(e3_protein_class),
    post_publication_update = as.character(`Updated/added post-publication`)
  )

message("Reading gnomAD v4.1.1 constraint table")
needed <- c(
  "gene", "gene_id", "transcript", "canonical", "mane_select", "transcript_version",
  "transcript_type", "transcript_level", "chromosome", "start_position", "end_position",
  "cds_length", "num_coding_exons",
  "gene_quality_metrics.exome_prop_bp_AN90", "gene_quality_metrics.exome_mean_AS_MQ",
  "gene_quality_metrics.exome_prop_segdup", "gene_quality_metrics.exome_prop_LCR",
  "gene_flags", "constraint_flags", "syn.obs", "syn.exp", "syn.oe",
  "mis.obs", "mis.exp", "mis.oe", "mis.z_score", "mis.oe_ci.lower", "mis.oe_ci.upper",
  "lof.obs", "lof.exp", "lof.oe", "lof.z_score", "lof.oe_ci.lower", "lof.oe_ci.upper",
  "lof.oe_ci.upper_rank", "lof.oe_ci.upper_bin_percentile", "lof.oe_ci.upper_bin_decile",
  "lof.pLI", "lof.pNull", "lof.pRec"
)

cmd <- paste("gunzip -c", shQuote(gnomad_raw))
gnomad <- fread(cmd = cmd, select = needed, na.strings = c("", "NA"))
gnomad_columns <- names(gnomad)

gnomad <- gnomad %>%
  mutate(
    ensembl_gene_id = clean_id(gene_id),
    transcript_id = clean_id(transcript),
    canonical_bool = is_true(canonical),
    mane_select_bool = is_true(mane_select),
    transcript_level_num = safe_num(transcript_level),
    cds_length_num = safe_num(cds_length),
    gene_symbol_key = toupper(as.character(gene)),
    gene_flags = ifelse(is.na(gene_flags), "[]", gene_flags),
    constraint_flags = ifelse(is.na(constraint_flags), "[]", constraint_flags)
  ) %>%
  filter(grepl("^ENSG", ensembl_gene_id), transcript_type == "protein_coding")

gnomad_selected <- gnomad %>%
  mutate(selection_rank = case_when(
    mane_select_bool ~ 1L,
    canonical_bool ~ 2L,
    TRUE ~ 3L
  )) %>%
  arrange(ensembl_gene_id, selection_rank, transcript_level_num, desc(cds_length_num), transcript_id) %>%
  group_by(ensembl_gene_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    transcript_selection_method = case_when(
      mane_select_bool ~ "MANE Select",
      canonical_bool ~ "Ensembl canonical",
      TRUE ~ "fallback transcript"
    ),
    quality_flags = paste(
      ifelse(empty_flag(gene_flags), "", paste0("gene_flags=", gene_flags)),
      ifelse(empty_flag(constraint_flags), "", paste0("constraint_flags=", constraint_flags)),
      sep = "; "
    ),
    quality_flags = trimws(gsub("^; |; $", "", quality_flags)),
    quality_flags = ifelse(quality_flags == "", "none", quality_flags),
    quality_status = ifelse(quality_flags == "none", "pass", "flagged"),
    low_coverage_flag = grepl("low|coverage", quality_flags, ignore.case = TRUE),
    poor_mappability_flag = grepl("map|segdup|lcr", quality_flags, ignore.case = TRUE),
    loeuf = safe_num(`lof.oe_ci.upper`),
    pLI = safe_num(`lof.pLI`),
    plof_obs = safe_num(`lof.obs`),
    plof_exp = safe_num(`lof.exp`),
    plof_oe = safe_num(`lof.oe`),
    missense_oe = safe_num(`mis.oe`),
    missense_z = safe_num(`mis.z_score`),
    synonymous_oe = safe_num(`syn.oe`),
    reliable_constraint = quality_status == "pass" & !is.na(loeuf)
  )

symbol_counts <- gnomad_selected %>%
  count(gene_symbol_key, name = "gnomad_symbol_matches")

e3_match <- e3 %>%
  left_join(symbol_counts, by = c("e3_symbol_key" = "gene_symbol_key")) %>%
  mutate(
    gnomad_symbol_matches = ifelse(is.na(gnomad_symbol_matches), 0L, gnomad_symbol_matches),
    match_status = case_when(
      gnomad_symbol_matches == 0L ~ "unmatched",
      gnomad_symbol_matches > 1L ~ "ambiguous_symbol",
      TRUE ~ "matched"
    )
  )

e3_joined <- e3_match %>%
  left_join(gnomad_selected, by = c("e3_symbol_key" = "gene_symbol_key")) %>%
  mutate(
    match_method = case_when(
      match_status == "matched" ~ "gene symbol checked fallback",
      match_status == "ambiguous_symbol" ~ "ambiguous gene symbol fallback",
      TRUE ~ "unmatched"
    ),
    match_note = case_when(
      match_status == "matched" ~ "E3 source has NCBI GeneID but no Ensembl gene ID; matched to one selected Ensembl protein-coding gnomAD row by symbol.",
      match_status == "ambiguous_symbol" ~ "E3 source has no Ensembl gene ID and symbol matched multiple selected gnomAD genes; do not interpret constraint.",
      TRUE ~ "No selected Ensembl protein-coding gnomAD v4.1.1 gene matched this E3 symbol."
    ),
    highly_lof_constrained = reliable_constraint & loeuf < 0.45,
    strong_missense_constraint = reliable_constraint & !is.na(missense_z) & missense_z >= 3.09,
    constraint_signal_pattern = case_when(
      !reliable_constraint ~ "missing or flagged constraint",
      highly_lof_constrained & strong_missense_constraint ~ "pLoF and missense constrained",
      highly_lof_constrained & !strong_missense_constraint ~ "pLoF constrained only",
      !highly_lof_constrained & strong_missense_constraint ~ "missense constrained only",
      TRUE ~ "no strong pLoF/missense signal"
    )
  )

duplicated_assignments <- e3_joined %>%
  filter(match_status == "matched") %>%
  count(ensembl_gene_id, name = "n_e3_records") %>%
  filter(n_e3_records > 1)

background <- gnomad_selected %>%
  mutate(is_e3 = ensembl_gene_id %in% e3_joined$ensembl_gene_id[e3_joined$match_status == "matched"]) %>%
  filter(reliable_constraint)

e3_analysis <- e3_joined %>%
  filter(match_status == "matched", reliable_constraint)

if (nrow(e3_analysis) < 2) stop("Too few matched, reliable E3 genes for analysis")

top15_cutoff <- quantile(background$loeuf, probs = 0.15, na.rm = TRUE, type = 7)
background <- background %>% mutate(top15_constrained = loeuf <= top15_cutoff)

e3_analysis <- e3_analysis %>%
  mutate(top15_constrained = loeuf <= top15_cutoff)

wil <- suppressWarnings(wilcox.test(e3_analysis$loeuf, background$loeuf, conf.int = TRUE))
e3_median <- median(e3_analysis$loeuf, na.rm = TRUE)
bg_median <- median(background$loeuf, na.rm = TRUE)
high_n <- sum(e3_analysis$highly_lof_constrained, na.rm = TRUE)
high_total <- nrow(e3_analysis)
high_ci <- wilson_ci(high_n, high_total)

tab <- table(
  E3 = background$is_e3,
  Top15 = background$top15_constrained
)
fish <- fisher.test(tab)

e3_class_memberships <- expand_class_memberships(e3_joined) %>%
  filter(major_e3_class != "Other/unknown")

class_summary <- e3_class_memberships %>%
  group_by(major_e3_class) %>%
  summarise(
    n_e3_input = n(),
    n_matched = sum(match_status == "matched", na.rm = TRUE),
    n_reliable_constraint = sum(match_status == "matched" & reliable_constraint, na.rm = TRUE),
    n_flagged_or_missing = sum(match_status == "matched" & !reliable_constraint, na.rm = TRUE),
    n_loeuf_lt_0_45 = sum(highly_lof_constrained, na.rm = TRUE),
    proportion_loeuf_lt_0_45 = ifelse(n_reliable_constraint > 0, n_loeuf_lt_0_45 / n_reliable_constraint, NA_real_),
    median_loeuf = median(loeuf[match_status == "matched" & reliable_constraint], na.rm = TRUE),
    median_missense_z = median(missense_z[match_status == "matched" & reliable_constraint], na.rm = TRUE),
    top_constrained_genes = paste(head(e3_gene_symbol[match_status == "matched" & reliable_constraint][order(loeuf[match_status == "matched" & reliable_constraint])], 8), collapse = "; "),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    loeuf_lt_0_45_ci_low = wilson_ci(n_loeuf_lt_0_45, n_reliable_constraint)[1],
    loeuf_lt_0_45_ci_high = wilson_ci(n_loeuf_lt_0_45, n_reliable_constraint)[2]
  ) %>%
  ungroup() %>%
  mutate(major_e3_class = factor(major_e3_class, levels = core_class_levels)) %>%
  arrange(major_e3_class) %>%
  mutate(major_e3_class = as.character(major_e3_class))

class_test_data <- expand_class_memberships(e3_analysis) %>%
  filter(!is.na(major_e3_class), !is.na(loeuf)) %>%
  filter(major_e3_class != "Other/unknown") %>%
  add_count(major_e3_class, name = "class_n") %>%
  filter(class_n >= 5)

class_plot_data <- expand_class_memberships(e3_analysis) %>%
  filter(!is.na(major_e3_class), !is.na(loeuf)) %>%
  filter(major_e3_class != "Other/unknown")

kw <- if (length(unique(class_test_data$major_e3_class)) > 1) {
  kruskal.test(loeuf ~ major_e3_class, data = class_test_data)
} else {
  NULL
}

pairwise <- data.frame()
classes <- core_class_levels[core_class_levels %in% unique(class_test_data$major_e3_class)]
if (length(classes) > 1) {
  pairs <- combn(classes, 2, simplify = FALSE)
  pairwise <- do.call(rbind, lapply(pairs, function(pr) {
    a <- class_test_data$loeuf[class_test_data$major_e3_class == pr[1]]
    b <- class_test_data$loeuf[class_test_data$major_e3_class == pr[2]]
    wt <- suppressWarnings(wilcox.test(a, b))
    data.frame(
      class_a = pr[1],
      class_b = pr[2],
      n_a = length(a),
      n_b = length(b),
      median_a = median(a),
      median_b = median(b),
      median_difference_a_minus_b = median(a) - median(b),
      p_value = wt$p.value
    )
  }))
  pairwise$p_value_bh <- p.adjust(pairwise$p_value, method = "BH")
}

ranked <- e3_joined %>%
  filter(match_status == "matched", !is.na(loeuf)) %>%
  arrange(loeuf, e3_gene_symbol) %>%
  mutate(loeuf_rank = row_number())

display_cols <- c(
  "input_order", "e3_gene_symbol", "ncbi_gene_id", "uniprot_accession",
  "e3_protein_class", "major_e3_class", "post_publication_update",
  "match_status", "match_method", "match_note", "gene", "ensembl_gene_id",
  "transcript_id", "transcript_version", "transcript_selection_method",
  "chromosome", "start_position", "end_position",
  "loeuf", "pLI", "plof_obs", "plof_exp", "plof_oe",
  "missense_oe", "missense_z", "synonymous_oe",
  "highly_lof_constrained", "strong_missense_constraint", "constraint_signal_pattern",
  "quality_status", "quality_flags", "low_coverage_flag", "poor_mappability_flag",
  "gene_quality_metrics.exome_prop_bp_AN90", "gene_quality_metrics.exome_mean_AS_MQ",
  "gene_quality_metrics.exome_prop_segdup", "gene_quality_metrics.exome_prop_LCR",
  "gene_flags", "constraint_flags"
)
display_cols <- intersect(display_cols, names(e3_joined))

match_quality <- e3_joined %>%
  transmute(
    input_order, e3_gene_symbol, ncbi_gene_id, uniprot_accession, e3_protein_class,
    major_e3_class, match_status, gnomad_symbol_matches, match_method, match_note,
    matched_gnomad_symbol = gene, ensembl_gene_id, transcript_id,
    duplicated_gene_assignment = ensembl_gene_id %in% duplicated_assignments$ensembl_gene_id,
    quality_status, quality_flags
  )

write.csv(e3_joined[, display_cols], file.path(root, "data", "e3_gnomad_constraint.csv"), row.names = FALSE, na = "")
write.csv(ranked[, intersect(c("loeuf_rank", display_cols), names(ranked))], file.path(root, "data", "e3_gnomad_ranked.csv"), row.names = FALSE, na = "")
write.csv(class_summary, file.path(root, "data", "e3_class_summary.csv"), row.names = FALSE, na = "")
write.csv(match_quality, file.path(root, "data", "match_quality_report.csv"), row.names = FALSE, na = "")
write.csv(pairwise, file.path(root, "data", "e3_class_pairwise_tests.csv"), row.names = FALSE, na = "")
write_simple_xlsx(e3_joined[, display_cols], file.path(root, "data", "e3_gnomad_constraint.xlsx"))

plot_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

dist_df <- bind_rows(
  background %>% transmute(group = "Protein-coding background", loeuf),
  e3_analysis %>% transmute(group = "E3-ome", loeuf)
)
p_dist <- ggplot(dist_df, aes(x = loeuf, fill = group, colour = group)) +
  geom_density(alpha = 0.24, linewidth = 0.7, na.rm = TRUE) +
  geom_vline(xintercept = 0.45, linetype = "dashed", colour = "#b42318") +
  scale_x_continuous(limits = c(0, quantile(dist_df$loeuf, 0.98, na.rm = TRUE))) +
  scale_fill_manual(values = c("E3-ome" = "#007c89", "Protein-coding background" = "#7a5c00")) +
  scale_colour_manual(values = c("E3-ome" = "#007c89", "Protein-coding background" = "#7a5c00")) +
  labs(title = "LOEUF distribution", x = "LOEUF (lower = stronger pLoF constraint)", y = "Density", fill = NULL, colour = NULL) +
  plot_theme

p_class <- ggplot(class_plot_data, aes(x = reorder(major_e3_class, loeuf, median), y = loeuf, fill = major_e3_class)) +
  geom_boxplot(outlier.alpha = 0.35, width = 0.7, na.rm = TRUE) +
  geom_hline(yintercept = 0.45, linetype = "dashed", colour = "#b42318") +
  coord_flip() +
  guides(fill = "none") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "LOEUF across major E3 classes", x = NULL, y = "LOEUF") +
  plot_theme

top25 <- ranked %>% filter(reliable_constraint) %>% slice_head(n = 25)
p_top <- ggplot(top25, aes(x = reorder(e3_gene_symbol, loeuf), y = loeuf)) +
  geom_col(width = 0.75, fill = "#176f6a") +
  geom_hline(yintercept = 0.45, linetype = "dashed", colour = "#b42318") +
  coord_flip() +
  labs(title = "Most pLoF-constrained E3 genes", x = NULL, y = "LOEUF") +
  plot_theme

scatter_data <- expand_class_memberships(e3_analysis) %>%
  filter(!is.na(major_e3_class), !is.na(loeuf), !is.na(missense_z)) %>%
  filter(major_e3_class != "Other/unknown")

p_scatter <- ggplot(scatter_data, aes(x = loeuf, y = missense_z, colour = major_e3_class)) +
  geom_point(alpha = 0.8, size = 2, na.rm = TRUE) +
  geom_vline(xintercept = 0.45, linetype = "dashed", colour = "#b42318") +
  geom_hline(yintercept = 3.09, linetype = "dotted", colour = "#5b5fc7") +
  labs(title = "pLoF and missense constraint signals", x = "LOEUF", y = "Missense Z-score", colour = "Family") +
  plot_theme

plots <- list(
  loeuf_distribution = p_dist,
  class_loeuf = p_class,
  top_constrained = p_top,
  loeuf_missense_scatter = p_scatter
)
for (nm in names(plots)) {
  grDevices::svg(file.path(root, "plots", paste0(nm, ".svg")), width = 8.5, height = 5.4)
  print(plots[[nm]])
  grDevices::dev.off()
  grDevices::png(file.path(root, "plots", paste0(nm, ".png")), width = 8.5, height = 5.4, units = "in", res = 300)
  print(plots[[nm]])
  grDevices::dev.off()
}

metadata <- list(
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  e3_source = list(url = e3_url, local_file = "data/raw/High-confidence 20260809.xlsx", version_label = "E3-ome gene list", input_rows = nrow(e3), columns = e3_columns),
  gnomad_source = list(url = gnomad_url, local_file = "data/raw/gnomad.v4.1.1.constraint_metrics.tsv.bgz", version_label = "gnomAD v4.1.1 constraint metrics", columns_read = gnomad_columns),
  matching = list(
    input_e3_genes = nrow(e3),
    unique_input_symbols = length(unique(e3$e3_symbol_key)),
    matched = sum(e3_match$match_status == "matched"),
    unmatched = sum(e3_match$match_status == "unmatched"),
    ambiguous = sum(e3_match$match_status == "ambiguous_symbol"),
    duplicated_gene_assignments = nrow(duplicated_assignments),
    primary_ensembl_matching_possible = FALSE,
    matching_note = "The E3 workbook does not contain Ensembl gene IDs; gnomAD matching therefore used checked gene-symbol fallback after selecting one Ensembl protein-coding transcript per gene."
  ),
  transcript_selection = list(
    background_genes_selected = nrow(gnomad_selected),
    mane_select = sum(gnomad_selected$transcript_selection_method == "MANE Select"),
    ensembl_canonical = sum(gnomad_selected$transcript_selection_method == "Ensembl canonical"),
    fallback_transcript = sum(gnomad_selected$transcript_selection_method == "fallback transcript")
  ),
  analysis = list(
    reliable_matched_e3_genes = nrow(e3_analysis),
    reliable_background_genes = nrow(background),
    loeuf_threshold_high_constraint = 0.45,
    e3_loeuf_lt_0_45 = high_n,
    e3_loeuf_lt_0_45_proportion = high_n / high_total,
    e3_loeuf_lt_0_45_ci_low = high_ci[1],
    e3_loeuf_lt_0_45_ci_high = high_ci[2],
    e3_median_loeuf = e3_median,
    background_median_loeuf = bg_median,
    median_difference_e3_minus_background = e3_median - bg_median,
    wilcoxon_p_value = wil$p.value,
    top15_loeuf_cutoff = unname(top15_cutoff),
    e3_in_top15 = sum(e3_analysis$top15_constrained),
    fisher_top15_odds_ratio = unname(fish$estimate),
    fisher_top15_ci_low = fish$conf.int[1],
    fisher_top15_ci_high = fish$conf.int[2],
    fisher_top15_p_value = fish$p.value,
    class_kruskal_p_value = if (is.null(kw)) NA_real_ else kw$p.value
  ),
  interpretation_limits = c(
    "Constraint reflects depletion of population variation, not direct proof of cellular essentiality.",
    "Constraint alone does not establish disease causality or therapeutic tractability.",
    "Missing or flagged constraint values are retained but excluded from inferential summaries."
  ),
  software = list(
    r_version = R.version.string,
    packages = sapply(c("data.table", "readxl", "dplyr", "ggplot2", "jsonlite"), function(pkg) as.character(packageVersion(pkg)))
  )
)

write_json(metadata, file.path(root, "data", "analysis_metadata.json"), pretty = TRUE, auto_unbox = TRUE, na = "null")

message("Done. Outputs written to ", root)
