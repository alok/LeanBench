declare const d3: any;

type MetricMap = Record<string, number>;

interface MetricSpec {
  key: string;
  label?: string;
  kind?: string;
  unit?: string;
  description?: string;
  group?: string;
  default_blend_weight?: number;
}

interface Span {
  file?: string;
  line?: number;
  col?: number;
  end_line?: number;
  end_col?: number;
}

interface NodeData {
  id: string;
  name?: string;
  path?: string;
  kind?: string;
  span?: Span;
  metrics?: MetricMap;
  tags?: string[];
  children?: NodeData[];
}

interface ObservatoryData {
  schema_version?: string;
  generated_at?: string;
  project?: {
    name?: string;
    root?: string;
    commit?: string;
    tool?: string;
  };
  metrics?: MetricSpec[];
  root: NodeData;
}

const fileInput = document.getElementById("fileInput") as HTMLInputElement | null;
const rootInput = document.getElementById("rootInput") as HTMLInputElement | null;
const sizeSelect = document.getElementById("sizeSelect") as HTMLSelectElement | null;
const colorSelect = document.getElementById("colorSelect") as HTMLSelectElement | null;
const listSelect = document.getElementById("listSelect") as HTMLSelectElement | null;
const listCount = document.getElementById("listCount") as HTMLInputElement | null;
const showLabels = document.getElementById("showLabels") as HTMLInputElement | null;
const tooltip = document.getElementById("tooltip") as HTMLDivElement | null;
const treemapEl = document.getElementById("treemap") as HTMLDivElement | null;
const blendPanel = document.getElementById("blendPanel") as HTMLDivElement | null;
const blendList = document.getElementById("blendList") as HTMLDivElement | null;
const topList = document.getElementById("topList") as HTMLDivElement | null;
const detailBody = document.getElementById("detailBody") as HTMLDivElement | null;

const BLEND_KEY = "__blend__";

const urlParams = new URLSearchParams(window.location.search);
const initialDataUrl = urlParams.get("data");
const initialRoot = urlParams.get("root");
const initialSize = urlParams.get("size");
const initialColor = urlParams.get("color");
const initialList = urlParams.get("list");
const initialListCount = urlParams.get("listCount");

const state: {
  data: ObservatoryData | null;
  sizeKey: string | null;
  colorKey: string | null;
  listKey: string | null;
  rootPrefix: string;
  metricSpecs: MetricSpec[];
  blendWeights: Record<string, number>;
  blendScores: Map<any, number> | null;
} = {
  data: null,
  sizeKey: null,
  colorKey: null,
  listKey: null,
  rootPrefix: "",
  metricSpecs: [],
  blendWeights: {},
  blendScores: null,
};

function collectMetrics(node: NodeData, set: Set<string>): void {
  if (node.metrics) {
    Object.keys(node.metrics).forEach((k) => set.add(k));
  }
  if (node.children) {
    node.children.forEach((c) => collectMetrics(c, set));
  }
}

function metricValue(node: NodeData | null, key: string | null): number {
  if (!node || !node.metrics || key == null) return 0;
  const val = node.metrics[key];
  return typeof val === "number" ? val : 0;
}

function labelForKey(key: string | null): string {
  if (key === BLEND_KEY) return "Blend";
  if (!key) return "(none)";
  const spec = state.metricSpecs.find((s) => s.key === key);
  if (spec && spec.label) return spec.label;
  return key;
}

function formatValue(value: number): string {
  if (!Number.isFinite(value)) return "0";
  if (Math.abs(value) >= 1000) return value.toLocaleString();
  return Number.isInteger(value) ? value.toString() : value.toFixed(3);
}

function initBlendWeights(specs: MetricSpec[]): void {
  const weights: Record<string, number> = {};
  specs.forEach((spec) => {
    const w = typeof spec.default_blend_weight === "number" ? spec.default_blend_weight : 0;
    weights[spec.key] = w;
  });
  const total = Object.values(weights).reduce((sum, v) => sum + v, 0);
  if (total === 0 && specs.length > 0) {
    weights[specs[0].key] = 25;
    if (specs[1]) weights[specs[1].key] = 25;
    if (specs[2]) weights[specs[2].key] = 25;
  }
  state.blendWeights = weights;
}

function renderBlendPanel(): void {
  if (!blendPanel || !blendList) return;
  const showBlend = state.colorKey === BLEND_KEY || state.listKey === BLEND_KEY;
  blendPanel.hidden = !showBlend;
  blendList.innerHTML = "";
  if (!showBlend) return;
  state.metricSpecs.forEach((spec) => {
    const item = document.createElement("div");
    item.className = "blend-item";
    const label = document.createElement("label");
    label.textContent = spec.label || spec.key;
    const row = document.createElement("div");
    row.className = "blend-row";
    const slider = document.createElement("input");
    slider.type = "range";
    slider.min = "0";
    slider.max = "100";
    slider.step = "5";
    slider.value = String(state.blendWeights[spec.key] ?? 0);
    slider.addEventListener("input", (event) => {
      const target = event.target as HTMLInputElement;
      state.blendWeights[spec.key] = Number(target.value);
      renderTreemap();
    });
    const value = document.createElement("div");
    value.className = "blend-value";
    value.textContent = slider.value;
    slider.addEventListener("input", () => {
      value.textContent = slider.value;
    });
    row.appendChild(slider);
    row.appendChild(value);
    item.appendChild(label);
    item.appendChild(row);
    blendList.appendChild(item);
  });
}

function updateMetricSelectors(): void {
  if (!sizeSelect || !colorSelect) return;
  let specs: MetricSpec[] = [];
  if (state.data && Array.isArray(state.data.metrics)) {
    specs = state.data.metrics.filter((m) => m && m.key);
  }
  if (specs.length === 0 && state.data && state.data.root) {
    const metrics = new Set<string>();
    collectMetrics(state.data.root, metrics);
    specs = Array.from(metrics)
      .sort()
      .map((k) => ({ key: k, label: k }));
  }
  state.metricSpecs = specs;

  const keys = specs.map((s) => s.key);
  sizeSelect.innerHTML = "";
  colorSelect.innerHTML = "";
  if (listSelect) listSelect.innerHTML = "";

  keys.forEach((k) => {
    const spec = specs.find((s) => s.key === k);
    const opt1 = document.createElement("option");
    opt1.value = k;
    opt1.textContent = spec?.label || k;
    sizeSelect.appendChild(opt1);
  });

  const blendOpt = document.createElement("option");
  blendOpt.value = BLEND_KEY;
  blendOpt.textContent = "Blend";
  colorSelect.appendChild(blendOpt);
  if (listSelect) listSelect.appendChild(blendOpt.cloneNode(true));

  keys.forEach((k) => {
    const spec = specs.find((s) => s.key === k);
    const opt2 = document.createElement("option");
    opt2.value = k;
    opt2.textContent = spec?.label || k;
    colorSelect.appendChild(opt2);
    if (listSelect) listSelect.appendChild(opt2.cloneNode(true));
  });

  state.sizeKey = keys.includes("size") ? "size" : keys[0] || null;

  initBlendWeights(specs);
  const hasBlendDefaults = Object.values(state.blendWeights).some((v) => v > 0);
  state.colorKey = hasBlendDefaults ? BLEND_KEY : keys[0] || null;
  state.listKey = state.colorKey || keys[0] || null;

  sizeSelect.value = state.sizeKey || "";
  colorSelect.value = state.colorKey || "";
  if (listSelect) listSelect.value = state.listKey || "";
  renderBlendPanel();
}

function applyMetricOverrides(): void {
  if (!sizeSelect || !colorSelect) return;
  if (initialSize) state.sizeKey = initialSize;
  if (initialColor) state.colorKey = initialColor;
  if (initialList) state.listKey = initialList;
  if (state.sizeKey) sizeSelect.value = state.sizeKey;
  if (state.colorKey) colorSelect.value = state.colorKey;
  if (listSelect && state.listKey) listSelect.value = state.listKey;
  if (listCount && initialListCount) listCount.value = initialListCount;
  renderBlendPanel();
}

function buildHierarchy() {
  if (!state.data) return null;
  const root = d3.hierarchy(state.data.root);
  root.sum((d: NodeData) => metricValue(d, state.sizeKey));
  root.sort((a: any, b: any) => b.value - a.value);
  return root;
}

function computeBlendScores(leaves: any[]): Map<any, number> {
  const active = Object.entries(state.blendWeights).filter(([, weight]) => weight > 0);
  const scores = new Map<any, number>();
  if (active.length === 0) {
    leaves.forEach((leaf) => scores.set(leaf, 0));
    return scores;
  }

  const ranges = active.map(([key, weight]) => {
    const values = leaves.map((leaf) => metricValue(leaf.data, key));
    const min = Math.min(...values);
    const max = Math.max(...values);
    return { key, weight, min, max };
  });

  leaves.forEach((leaf) => {
    let total = 0;
    ranges.forEach(({ key, weight, min, max }) => {
      const value = metricValue(leaf.data, key);
      const norm = max === min ? 0 : (value - min) / (max - min);
      total += weight * norm;
    });
    scores.set(leaf, total);
  });

  return scores;
}

function listMetricValue(node: NodeData, leaf: any): number {
  if (state.listKey === BLEND_KEY) {
    return state.blendScores?.get(leaf) || 0;
  }
  return metricValue(node, state.listKey);
}

function renderTopList(leaves: any[]): void {
  if (!topList) return;
  const limit = Math.max(5, Math.min(200, Number(listCount?.value || 30)));
  const sorted = [...leaves].sort((a, b) => listMetricValue(b.data, b) - listMetricValue(a.data, a));
  topList.innerHTML = "";
  sorted.slice(0, limit).forEach((leaf, idx) => {
    const item = document.createElement("div");
    item.className = "top-item";
    item.addEventListener("click", () => openInEditor(leaf.data));
    item.addEventListener("mouseenter", () => renderDetails(leaf.data));

    const rank = document.createElement("div");
    rank.className = "top-rank";
    rank.textContent = `#${idx + 1}`;

    const name = document.createElement("div");
    name.className = "top-name";
    name.textContent = leaf.data.name || leaf.data.id || "(unnamed)";

    const path = document.createElement("div");
    path.className = "top-path";
    path.textContent = leaf.data.path || "";
    name.appendChild(path);

    const value = document.createElement("div");
    value.className = "top-value";
    value.textContent = formatValue(listMetricValue(leaf.data, leaf));

    item.appendChild(rank);
    item.appendChild(name);
    item.appendChild(value);
    topList.appendChild(item);
  });
}

function renderDetails(node: NodeData | null): void {
  if (!detailBody) return;
  detailBody.innerHTML = "";
  if (!node) {
    detailBody.textContent = "Hover a tile to inspect metrics.";
    return;
  }
  const title = document.createElement("div");
  title.className = "detail-title";
  title.textContent = node.name || node.id || "(unnamed)";

  const path = document.createElement("div");
  path.className = "detail-path";
  path.textContent = node.path || "";

  const metrics = node.metrics ? Object.entries(node.metrics) : [];
  metrics.sort((a, b) => (b[1] || 0) - (a[1] || 0));
  const list = document.createElement("div");
  list.className = "detail-metrics";
  metrics.slice(0, 14).forEach(([key, value]) => {
    const row = document.createElement("div");
    row.className = "detail-row";
    const label = document.createElement("span");
    label.textContent = labelForKey(key);
    const val = document.createElement("strong");
    val.textContent = formatValue(value);
    row.appendChild(label);
    row.appendChild(val);
    list.appendChild(row);
  });
  if (metrics.length > 14) {
    const more = document.createElement("div");
    more.className = "detail-row";
    more.innerHTML = `<span>…</span><strong>${metrics.length - 14} more</strong>`;
    list.appendChild(more);
  }

  detailBody.appendChild(title);
  detailBody.appendChild(path);
  detailBody.appendChild(list);
}

function renderTreemap(): void {
  if (!state.data || !treemapEl || !state.sizeKey || !state.colorKey) return;

  treemapEl.innerHTML = "";
  const width = treemapEl.clientWidth;
  const height = treemapEl.clientHeight;

  const root = buildHierarchy();
  if (!root) return;
  d3.treemap().size([width, height]).paddingInner(1)(root);

  const leaves = root.leaves();
  let values: number[] = [];
  let colorAccessor: (leaf: any) => number;

  const needsBlend = state.colorKey === BLEND_KEY || state.listKey === BLEND_KEY;
  if (needsBlend) {
    const scores = computeBlendScores(leaves);
    state.blendScores = scores;
    values = leaves.map((leaf: any) => scores.get(leaf) || 0);
    colorAccessor = (leaf: any) => scores.get(leaf) || 0;
  } else {
    state.blendScores = null;
    values = leaves.map((leaf: any) => metricValue(leaf.data, state.colorKey));
    colorAccessor = (leaf: any) => metricValue(leaf.data, state.colorKey);
  }

  const min = Math.min(...values);
  const max = Math.max(...values);
  const scale = d3.scaleSequential(d3.interpolateRdYlBu).domain([max, min || 1]);

  const svg = d3
    .select(treemapEl)
    .append("svg")
    .attr("width", width)
    .attr("height", height);

  const nodes = svg
    .selectAll("g")
    .data(leaves)
    .enter()
    .append("g")
    .attr("transform", (d: any) => `translate(${d.x0},${d.y0})`);

  nodes
    .append("rect")
    .attr("class", "tile")
    .attr("width", (d: any) => d.x1 - d.x0)
    .attr("height", (d: any) => d.y1 - d.y0)
    .attr("fill", (d: any) => scale(colorAccessor(d)))
    .on("mousemove", (event: MouseEvent, d: any) => showTooltip(event, d))
    .on("mouseleave", hideTooltip)
    .on("click", (_event: MouseEvent, d: any) => openInEditor(d.data));

  if (showLabels && showLabels.checked) {
    nodes
      .append("text")
      .attr("class", "label")
      .attr("x", 6)
      .attr("y", 14)
      .text((d: any) => d.data.name || d.data.id)
      .each(function (this: SVGTextElement, d: any) {
        const nodeWidth = d.x1 - d.x0;
        const nodeHeight = d.y1 - d.y0;
        if (nodeWidth < 80 || nodeHeight < 18) {
          d3.select(this).attr("display", "none");
        }
      });
  }

  renderTopList(leaves);
}

function showTooltip(event: MouseEvent, d: any): void {
  if (!tooltip) return;
  const name = d.data.name || d.data.id || "(unnamed)";
  const path = d.data.path || "";
  const sizeVal = metricValue(d.data, state.sizeKey);
  const colorVal =
    state.colorKey === BLEND_KEY ? state.blendScores?.get(d) || 0 : metricValue(d.data, state.colorKey);
  tooltip.innerHTML = `
    <div><strong>${name}</strong></div>
    <div>${path}</div>
    <div>size: ${labelForKey(state.sizeKey)} = ${formatValue(sizeVal)}</div>
    <div>color: ${labelForKey(state.colorKey)} = ${formatValue(colorVal)}</div>
  `;
  tooltip.hidden = false;
  tooltip.style.left = `${event.clientX + 12}px`;
  tooltip.style.top = `${event.clientY + 12}px`;
  renderDetails(d.data);
}

function hideTooltip(): void {
  if (!tooltip) return;
  tooltip.hidden = true;
}

function openInEditor(node: NodeData): void {
  const span = node.span || {};
  const line = span.line || 1;
  const col = span.col || 1;
  let file = node.path || "";
  if (!file) return;
  if (!file.startsWith("/") && state.rootPrefix) {
    const prefix = state.rootPrefix.replace(/\/$/, "");
    file = `${prefix}/${file}`;
  }
  const url = `vscode://file/${file}:${line}:${col}`;
  window.open(url, "_blank");
}

fileInput?.addEventListener("change", async (event) => {
  const target = event.target as HTMLInputElement;
  const file = target.files ? target.files[0] : null;
  if (!file) return;
  const text = await file.text();
  const parsed = JSON.parse(text) as ObservatoryData;
  state.data = parsed;
  if (!state.rootPrefix && parsed.project?.root && parsed.project.root.startsWith("/")) {
    state.rootPrefix = parsed.project.root;
    if (rootInput) rootInput.value = state.rootPrefix;
  }
  updateMetricSelectors();
  renderTreemap();
});

async function loadFromUrl(url: string): Promise<void> {
  const resolved = new URL(url, window.location.href).toString();
  const resp = await fetch(resolved);
  const parsed = (await resp.json()) as ObservatoryData;
  state.data = parsed;
  if (!state.rootPrefix && parsed.project?.root && parsed.project.root.startsWith("/")) {
    state.rootPrefix = parsed.project.root;
    if (rootInput) rootInput.value = state.rootPrefix;
  }
  updateMetricSelectors();
  applyMetricOverrides();
  renderTreemap();
}

if (initialRoot && rootInput) {
  state.rootPrefix = initialRoot;
  rootInput.value = initialRoot;
}

if (initialDataUrl) {
  loadFromUrl(initialDataUrl).catch((err) => {
    console.error(err);
  });
}

rootInput?.addEventListener("input", (event) => {
  const target = event.target as HTMLInputElement;
  state.rootPrefix = target.value.trim();
});

sizeSelect?.addEventListener("change", (event) => {
  const target = event.target as HTMLSelectElement;
  state.sizeKey = target.value;
  renderTreemap();
});

colorSelect?.addEventListener("change", (event) => {
  const target = event.target as HTMLSelectElement;
  state.colorKey = target.value;
  renderBlendPanel();
  renderTreemap();
});

listSelect?.addEventListener("change", (event) => {
  const target = event.target as HTMLSelectElement;
  state.listKey = target.value;
  renderBlendPanel();
  renderTreemap();
});

listCount?.addEventListener("input", () => renderTreemap());
showLabels?.addEventListener("change", () => renderTreemap());

window.addEventListener("resize", () => {
  if (state.data) renderTreemap();
});
