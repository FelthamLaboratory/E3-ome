const state = {
  rows: [],
  classRows: [],
  metadata: null,
  sortKey: "loeuf",
  sortDir: "asc"
};

const tableColumns = [
  ["e3_gene_symbol", "Gene"],
  ["major_e3_class", "Families"],
  ["loeuf", "LOEUF"],
  ["pLI", "pLI"],
  ["plof_obs", "pLoF obs"],
  ["plof_exp", "pLoF exp"],
  ["plof_oe", "pLoF O/E"],
  ["missense_oe", "Mis O/E"],
  ["missense_z", "Mis Z"],
  ["transcript_selection_method", "Transcript"],
  ["quality_status", "Quality"],
  ["constraint_signal_pattern", "Signal"],
  ["ensembl_gene_id", "Ensembl"],
  ["transcript_id", "Transcript ID"]
];

const classColumns = [
  ["major_e3_class", "Class"],
  ["n_e3_input", "Input"],
  ["n_matched", "Matched"],
  ["n_reliable_constraint", "Reliable"],
  ["n_loeuf_lt_0_45", "LOEUF < 0.45"],
  ["proportion_loeuf_lt_0_45", "Proportion"],
  ["median_loeuf", "Median LOEUF"],
  ["median_missense_z", "Median Mis Z"],
  ["top_constrained_genes", "Top genes"]
];

function parseCSV(text) {
  const rows = [];
  let row = [];
  let value = "";
  let quoted = false;
  for (let i = 0; i < text.length; i += 1) {
    const char = text[i];
    const next = text[i + 1];
    if (quoted) {
      if (char === '"' && next === '"') {
        value += '"';
        i += 1;
      } else if (char === '"') {
        quoted = false;
      } else {
        value += char;
      }
    } else if (char === '"') {
      quoted = true;
    } else if (char === ",") {
      row.push(value);
      value = "";
    } else if (char === "\n") {
      row.push(value);
      rows.push(row);
      row = [];
      value = "";
    } else if (char !== "\r") {
      value += char;
    }
  }
  if (value || row.length) {
    row.push(value);
    rows.push(row);
  }
  const headers = rows.shift() || [];
  return rows.filter(r => r.length && r.some(Boolean)).map(r => {
    const obj = {};
    headers.forEach((h, i) => {
      obj[h] = r[i] ?? "";
    });
    return obj;
  });
}

function asNum(value) {
  const n = Number(value);
  return Number.isFinite(n) ? n : null;
}

function fmt(value, digits = 3) {
  const n = asNum(value);
  if (n === null) return value || "";
  if (Math.abs(n) < 0.001 && n !== 0) return n.toExponential(2);
  return n.toLocaleString(undefined, { maximumFractionDigits: digits });
}

function pct(value) {
  const n = asNum(value);
  return n === null ? "" : `${(n * 100).toFixed(1)}%`;
}

function pval(value) {
  const n = asNum(value);
  if (n === null) return "";
  return n < 0.001 ? n.toExponential(2) : n.toFixed(3);
}

function badge(text, kind) {
  return `<span class="badge ${kind}">${text}</span>`;
}

function setText(id, value) {
  const element = document.getElementById(id);
  if (element) element.textContent = value;
}

function populateSelect(id, values) {
  const select = document.getElementById(id);
  values.filter(Boolean).sort().forEach(value => {
    const option = document.createElement("option");
    option.value = value;
    option.textContent = value;
    select.appendChild(option);
  });
}

function renderSummary() {
  const md = state.metadata;
  const m = md.matching || {};
  const a = md.analysis || {};
  setText("inputGenes", fmt(m.input_e3_genes, 0));
  setText("matchedGenes", `${fmt(m.matched, 0)} matched`);
  setText("highConstraint", `${fmt(a.e3_loeuf_lt_0_45, 0)} (${pct(a.e3_loeuf_lt_0_45_proportion)})`);
  setText("enrichment", `OR ${fmt(a.fisher_top15_odds_ratio, 2)}, P ${pval(a.fisher_top15_p_value)}`);
  setText("e3Median", fmt(a.e3_median_loeuf));
  setText("bgMedian", fmt(a.background_median_loeuf));
  setText("wilcox", `P ${pval(a.wilcoxon_p_value)}`);
  setText("classTest", `P ${pval(a.class_kruskal_p_value)}`);
  setText("generatedAt", `Generated ${md.generated_at}. gnomAD v4.1.1 and the E3-ome.`);
  setText("findingText", `${fmt(a.reliable_matched_e3_genes, 0)} matched E3 genes had reliable LOEUF values. ${fmt(a.e3_loeuf_lt_0_45, 0)} were below the gnomAD v4.1.1 high-constraint threshold of 0.45.`);
}

function sortRows(rows) {
  const key = state.sortKey;
  const dir = state.sortDir === "asc" ? 1 : -1;
  return rows.slice().sort((a, b) => {
    const an = asNum(a[key]);
    const bn = asNum(b[key]);
    let cmp;
    if (an !== null && bn !== null) cmp = an - bn;
    else cmp = String(a[key] || "").localeCompare(String(b[key] || ""));
    return cmp * dir;
  });
}

function filteredRows() {
  const q = document.getElementById("search").value.trim().toLowerCase();
  const cls = document.getElementById("classFilter").value;
  const loeuf = document.getElementById("loeufFilter").value;
  const tx = document.getElementById("transcriptFilter").value;
  const qual = document.getElementById("qualityFilter").value;
  const top15 = state.metadata && state.metadata.analysis ? state.metadata.analysis.top15_loeuf_cutoff : null;
  return state.rows.filter(row => {
    const haystack = [
      row.e3_gene_symbol,
      row.major_e3_class,
      row.e3_protein_class,
      row.ensembl_gene_id,
      row.transcript_id,
      row.constraint_signal_pattern
    ].join(" ").toLowerCase();
    if (q && !haystack.includes(q)) return false;
    const memberships = String(row.major_e3_class || "").split(";").map(x => x.trim());
    if (cls && !memberships.includes(cls)) return false;
    if (tx && row.transcript_selection_method !== tx) return false;
    if (qual && row.quality_status !== qual) return false;
    const l = asNum(row.loeuf);
    if (loeuf === "lt045" && !(l !== null && l < 0.45 && row.quality_status === "pass")) return false;
    if (loeuf === "top15" && !(l !== null && top15 !== null && l <= top15 && row.quality_status === "pass")) return false;
    if (loeuf === "missing" && row.quality_status === "pass" && l !== null) return false;
    return true;
  });
}

function renderGeneTable() {
  const table = document.getElementById("geneTable");
  table.tHead.innerHTML = `<tr>${tableColumns.map(([key, label]) => `<th data-key="${key}">${label}</th>`).join("")}</tr>`;
  table.tHead.querySelectorAll("th").forEach(th => {
    th.addEventListener("click", () => {
      const key = th.dataset.key;
      state.sortDir = state.sortKey === key && state.sortDir === "asc" ? "desc" : "asc";
      state.sortKey = key;
      renderGeneTable();
    });
  });
  const rows = sortRows(filteredRows());
  document.getElementById("resultCount").textContent = `${rows.length.toLocaleString()} of ${state.rows.length.toLocaleString()} E3 records shown`;
  table.tBodies[0].innerHTML = rows.map(row => {
    return `<tr>${tableColumns.map(([key]) => {
      let val = row[key] || "NA";
      if (["loeuf", "pLI", "plof_oe", "missense_oe", "missense_z"].includes(key)) val = fmt(val);
      if (["plof_obs", "plof_exp"].includes(key)) val = fmt(val, 1);
      if (key === "quality_status") {
        val = row.quality_status === "pass" ? badge("pass", "pass") : badge(row.quality_status || "flagged", "flagged");
      }
      if (key === "loeuf" && row.quality_status === "pass" && asNum(row.loeuf) !== null && asNum(row.loeuf) < 0.45) {
        val = badge(fmt(row.loeuf), "high");
      }
      if (val === "") val = "NA";
      return `<td>${val}</td>`;
    }).join("")}</tr>`;
  }).join("");
}

function renderClassTable() {
  const table = document.getElementById("classTable");
  table.tHead.innerHTML = `<tr>${classColumns.map(([, label]) => `<th>${label}</th>`).join("")}</tr>`;
  table.tBodies[0].innerHTML = state.classRows.map(row => {
    return `<tr>${classColumns.map(([key]) => {
      let val = row[key] || "NA";
      if (key === "proportion_loeuf_lt_0_45") val = pct(val);
      if (["median_loeuf", "median_missense_z"].includes(key)) val = fmt(val);
      if (val === "") val = "NA";
      return `<td>${val}</td>`;
    }).join("")}</tr>`;
  }).join("");
}

async function init() {
  const [geneText, classText, metadata] = await Promise.all([
    fetch("data/e3_gnomad_constraint.csv").then(r => r.text()),
    fetch("data/e3_class_summary.csv").then(r => r.text()),
    fetch("data/analysis_metadata.json").then(r => r.json())
  ]);
  state.rows = parseCSV(geneText);
  state.classRows = parseCSV(classText);
  state.metadata = metadata;
  populateSelect("classFilter", Array.from(new Set(state.classRows.map(r => r.major_e3_class))));
  populateSelect("transcriptFilter", Array.from(new Set(state.rows.map(r => r.transcript_selection_method))));
  populateSelect("qualityFilter", Array.from(new Set(state.rows.map(r => r.quality_status))));
  renderSummary();
  renderClassTable();
  renderGeneTable();
  document.querySelectorAll("#filters input, #filters select").forEach(el => {
    el.addEventListener("input", renderGeneTable);
    el.addEventListener("change", renderGeneTable);
  });
}

init().catch(error => {
  console.error(error);
  document.getElementById("resultCount").textContent = "Unable to load the local data files. Use GitHub Pages or run: python3 -m http.server 8000 --directory Genetics/gnomAD";
});
