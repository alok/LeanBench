"use strict";
const fileInput = document.getElementById("fileInput");
const rootInput = document.getElementById("rootInput");
const sizeSelect = document.getElementById("sizeSelect");
const colorSelect = document.getElementById("colorSelect");
const listSelect = document.getElementById("listSelect");
const listCount = document.getElementById("listCount");
const showLabels = document.getElementById("showLabels");
const tooltip = document.getElementById("tooltip");
const treemapEl = document.getElementById("treemap");
const blendPanel = document.getElementById("blendPanel");
const blendList = document.getElementById("blendList");
const topList = document.getElementById("topList");
const detailBody = document.getElementById("detailBody");
const BLEND_KEY = "__blend__";
const state = {
    data: null,
    sizeKey: null,
    colorKey: null,
    listKey: null,
    rootPrefix: "",
    metricSpecs: [],
    blendWeights: {},
    blendScores: null,
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
    if (!node || !node.metrics || key == null)
        return 0;
    const val = node.metrics[key];
    return typeof val === "number" ? val : 0;
}
function labelForKey(key) {
    if (key === BLEND_KEY)
        return "Blend";
    if (!key)
        return "(none)";
    const spec = state.metricSpecs.find((s) => s.key === key);
    if (spec && spec.label)
        return spec.label;
    return key;
}
function formatValue(value) {
    if (!Number.isFinite(value))
        return "0";
    if (Math.abs(value) >= 1000)
        return value.toLocaleString();
    return Number.isInteger(value) ? value.toString() : value.toFixed(3);
}
function initBlendWeights(specs) {
    const weights = {};
    specs.forEach((spec) => {
        const w = typeof spec.default_blend_weight === "number" ? spec.default_blend_weight : 0;
        weights[spec.key] = w;
    });
    const total = Object.values(weights).reduce((sum, v) => sum + v, 0);
    if (total === 0 && specs.length > 0) {
        weights[specs[0].key] = 25;
        if (specs[1])
            weights[specs[1].key] = 25;
        if (specs[2])
            weights[specs[2].key] = 25;
    }
    state.blendWeights = weights;
}
function renderBlendPanel() {
    if (!blendPanel || !blendList)
        return;
    const showBlend = state.colorKey === BLEND_KEY || state.listKey === BLEND_KEY;
    blendPanel.hidden = !showBlend;
    blendList.innerHTML = "";
    if (!showBlend)
        return;
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
            const target = event.target;
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
function updateMetricSelectors() {
    if (!sizeSelect || !colorSelect)
        return;
    let specs = [];
    if (state.data && Array.isArray(state.data.metrics)) {
        specs = state.data.metrics.filter((m) => m && m.key);
    }
    if (specs.length === 0 && state.data && state.data.root) {
        const metrics = new Set();
        collectMetrics(state.data.root, metrics);
        specs = Array.from(metrics)
            .sort()
            .map((k) => ({ key: k, label: k }));
    }
    state.metricSpecs = specs;
    const keys = specs.map((s) => s.key);
    sizeSelect.innerHTML = "";
    colorSelect.innerHTML = "";
    if (listSelect)
        listSelect.innerHTML = "";
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
    if (listSelect)
        listSelect.appendChild(blendOpt.cloneNode(true));
    keys.forEach((k) => {
        const spec = specs.find((s) => s.key === k);
        const opt2 = document.createElement("option");
        opt2.value = k;
        opt2.textContent = spec?.label || k;
        colorSelect.appendChild(opt2);
        if (listSelect)
            listSelect.appendChild(opt2.cloneNode(true));
    });
    state.sizeKey = keys.includes("size") ? "size" : keys[0] || null;
    initBlendWeights(specs);
    const hasBlendDefaults = Object.values(state.blendWeights).some((v) => v > 0);
    state.colorKey = hasBlendDefaults ? BLEND_KEY : keys[0] || null;
    state.listKey = state.colorKey || keys[0] || null;
    sizeSelect.value = state.sizeKey || "";
    colorSelect.value = state.colorKey || "";
    if (listSelect)
        listSelect.value = state.listKey || "";
    renderBlendPanel();
}
function buildHierarchy() {
    if (!state.data)
        return null;
    const root = d3.hierarchy(state.data.root);
    root.sum((d) => metricValue(d, state.sizeKey));
    root.sort((a, b) => b.value - a.value);
    return root;
}
function computeBlendScores(leaves) {
    const active = Object.entries(state.blendWeights).filter(([, weight]) => weight > 0);
    const scores = new Map();
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
function listMetricValue(node, leaf) {
    if (state.listKey === BLEND_KEY) {
        return state.blendScores?.get(leaf) || 0;
    }
    return metricValue(node, state.listKey);
}
function renderTopList(leaves) {
    if (!topList)
        return;
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
function renderDetails(node) {
    if (!detailBody)
        return;
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
function renderTreemap() {
    if (!state.data || !treemapEl || !state.sizeKey || !state.colorKey)
        return;
    treemapEl.innerHTML = "";
    const width = treemapEl.clientWidth;
    const height = treemapEl.clientHeight;
    const root = buildHierarchy();
    if (!root)
        return;
    d3.treemap().size([width, height]).paddingInner(1)(root);
    const leaves = root.leaves();
    let values = [];
    let colorAccessor;
    const needsBlend = state.colorKey === BLEND_KEY || state.listKey === BLEND_KEY;
    if (needsBlend) {
        const scores = computeBlendScores(leaves);
        state.blendScores = scores;
        values = leaves.map((leaf) => scores.get(leaf) || 0);
        colorAccessor = (leaf) => scores.get(leaf) || 0;
    }
    else {
        state.blendScores = null;
        values = leaves.map((leaf) => metricValue(leaf.data, state.colorKey));
        colorAccessor = (leaf) => metricValue(leaf.data, state.colorKey);
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
        .attr("transform", (d) => `translate(${d.x0},${d.y0})`);
    nodes
        .append("rect")
        .attr("class", "tile")
        .attr("width", (d) => d.x1 - d.x0)
        .attr("height", (d) => d.y1 - d.y0)
        .attr("fill", (d) => scale(colorAccessor(d)))
        .on("mousemove", (event, d) => showTooltip(event, d))
        .on("mouseleave", hideTooltip)
        .on("click", (_event, d) => openInEditor(d.data));
    if (showLabels && showLabels.checked) {
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
    renderTopList(leaves);
}
function showTooltip(event, d) {
    if (!tooltip)
        return;
    const name = d.data.name || d.data.id || "(unnamed)";
    const path = d.data.path || "";
    const sizeVal = metricValue(d.data, state.sizeKey);
    const colorVal = state.colorKey === BLEND_KEY ? state.blendScores?.get(d) || 0 : metricValue(d.data, state.colorKey);
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
function hideTooltip() {
    if (!tooltip)
        return;
    tooltip.hidden = true;
}
function openInEditor(node) {
    const span = node.span || {};
    const line = span.line || 1;
    const col = span.col || 1;
    let file = node.path || "";
    if (!file)
        return;
    if (!file.startsWith("/") && state.rootPrefix) {
        const prefix = state.rootPrefix.replace(/\/$/, "");
        file = `${prefix}/${file}`;
    }
    const url = `vscode://file/${file}:${line}:${col}`;
    window.open(url, "_blank");
}
fileInput?.addEventListener("change", async (event) => {
    const target = event.target;
    const file = target.files ? target.files[0] : null;
    if (!file)
        return;
    const text = await file.text();
    const parsed = JSON.parse(text);
    state.data = parsed;
    if (!state.rootPrefix && parsed.project?.root && parsed.project.root.startsWith("/")) {
        state.rootPrefix = parsed.project.root;
        if (rootInput)
            rootInput.value = state.rootPrefix;
    }
    updateMetricSelectors();
    renderTreemap();
});
rootInput?.addEventListener("input", (event) => {
    const target = event.target;
    state.rootPrefix = target.value.trim();
});
sizeSelect?.addEventListener("change", (event) => {
    const target = event.target;
    state.sizeKey = target.value;
    renderTreemap();
});
colorSelect?.addEventListener("change", (event) => {
    const target = event.target;
    state.colorKey = target.value;
    renderBlendPanel();
    renderTreemap();
});
listSelect?.addEventListener("change", (event) => {
    const target = event.target;
    state.listKey = target.value;
    renderBlendPanel();
    renderTreemap();
});
listCount?.addEventListener("input", () => renderTreemap());
showLabels?.addEventListener("change", () => renderTreemap());
window.addEventListener("resize", () => {
    if (state.data)
        renderTreemap();
});
