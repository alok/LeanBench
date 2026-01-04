const fileInput = document.getElementById("fileInput");
const rootInput = document.getElementById("rootInput");
const sizeSelect = document.getElementById("sizeSelect");
const colorSelect = document.getElementById("colorSelect");
const showLabels = document.getElementById("showLabels");
const tooltip = document.getElementById("tooltip");
const treemapEl = document.getElementById("treemap");

const state = {
  data: null,
  sizeKey: null,
  colorKey: null,
  rootPrefix: "",
};

function collectMetrics(node, set) {
  if (node.metrics) {
    Object.keys(node.metrics).forEach((k) => set.add(k));
  }
  if (node.children) {
    node.children.forEach((c) => collectMetrics(c, set));
  }
}

function metricValue(node, key) {
  if (!node || !node.metrics || key == null) return 0;
  const val = node.metrics[key];
  return typeof val === "number" ? val : 0;
}

function updateMetricSelectors() {
  const metrics = new Set();
  collectMetrics(state.data.root, metrics);
  const list = Array.from(metrics).sort();
  sizeSelect.innerHTML = "";
  colorSelect.innerHTML = "";
  list.forEach((k) => {
    const opt1 = document.createElement("option");
    opt1.value = k;
    opt1.textContent = k;
    sizeSelect.appendChild(opt1);
    const opt2 = document.createElement("option");
    opt2.value = k;
    opt2.textContent = k;
    colorSelect.appendChild(opt2);
  });
  state.sizeKey = list[0] || null;
  state.colorKey = list[0] || null;
  sizeSelect.value = state.sizeKey || "";
  colorSelect.value = state.colorKey || "";
}

function buildHierarchy() {
  const root = d3.hierarchy(state.data.root);
  root.sum((d) => metricValue(d, state.sizeKey));
  root.sort((a, b) => b.value - a.value);
  return root;
}

function renderTreemap() {
  if (!state.data) return;

  treemapEl.innerHTML = "";
  const width = treemapEl.clientWidth;
  const height = treemapEl.clientHeight;

  const root = buildHierarchy();
  d3.treemap().size([width, height]).paddingInner(1)(root);

  const leaves = root.leaves();
  const values = leaves.map((d) => metricValue(d.data, state.colorKey));
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
    .attr("transform", (d) => `translate(${d.x0},${d.y0})`);

  nodes
    .append("rect")
    .attr("class", "tile")
    .attr("width", (d) => d.x1 - d.x0)
    .attr("height", (d) => d.y1 - d.y0)
    .attr("fill", (d) => scale(metricValue(d.data, state.colorKey)))
    .on("mousemove", (event, d) => showTooltip(event, d))
    .on("mouseleave", hideTooltip)
    .on("click", (event, d) => openInEditor(d.data));

  if (showLabels.checked) {
    nodes
      .append("text")
      .attr("class", "label")
      .attr("x", 6)
      .attr("y", 14)
      .text((d) => d.data.name || d.data.id)
      .each(function (d) {
        const nodeWidth = d.x1 - d.x0;
        const nodeHeight = d.y1 - d.y0;
        if (nodeWidth < 80 || nodeHeight < 18) {
          d3.select(this).attr("display", "none");
        }
      });
  }
}

function showTooltip(event, d) {
  const name = d.data.name || d.data.id || "(unnamed)";
  const path = d.data.path || "";
  const sizeVal = metricValue(d.data, state.sizeKey);
  const colorVal = metricValue(d.data, state.colorKey);
  tooltip.innerHTML = `
    <div><strong>${name}</strong></div>
    <div>${path}</div>
    <div>size: ${state.sizeKey} = ${sizeVal}</div>
    <div>color: ${state.colorKey} = ${colorVal}</div>
  `;
  tooltip.hidden = false;
  tooltip.style.left = `${event.clientX + 12}px`;
  tooltip.style.top = `${event.clientY + 12}px`;
}

function hideTooltip() {
  tooltip.hidden = true;
}

function openInEditor(node) {
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

fileInput.addEventListener("change", async (event) => {
  const file = event.target.files[0];
  if (!file) return;
  const text = await file.text();
  state.data = JSON.parse(text);
  updateMetricSelectors();
  renderTreemap();
});

rootInput.addEventListener("input", (event) => {
  state.rootPrefix = event.target.value.trim();
});

sizeSelect.addEventListener("change", (event) => {
  state.sizeKey = event.target.value;
  renderTreemap();
});

colorSelect.addEventListener("change", (event) => {
  state.colorKey = event.target.value;
  renderTreemap();
});

showLabels.addEventListener("change", () => renderTreemap());

window.addEventListener("resize", () => {
  if (state.data) renderTreemap();
});
