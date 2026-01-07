"use strict";
/** Metric group metadata for organizing selectors. */
const METRIC_GROUPS = {
    textscan: { label: "Compile-time (Text)", order: 0 },
    infotree: { label: "Compile-time (Semantic)", order: 1 },
    profiler: { label: "Compile-time (Profiler)", order: 2 },
    cpu: { label: "Runtime: CPU", order: 3 },
    gpu: { label: "Runtime: GPU", order: 4 },
    memory: { label: "Runtime: Memory", order: 5 },
    ffi: { label: "Runtime: FFI", order: 6 },
    other: { label: "Other", order: 99 },
};
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
// View switching elements
const viewTabs = document.querySelectorAll(".view-tab");
const treemapView = document.getElementById("treemapView");
const timelineView = document.getElementById("timelineView");
const allocsView = document.getElementById("allocsView");
const timelineEl = document.getElementById("timeline");
const allocsEl = document.getElementById("allocs");
const timelineColorBy = document.getElementById("timelineColorBy");
const allocsGroupBy = document.getElementById("allocsGroupBy");
const BLEND_KEY = "__blend__";
const COMPILE_GROUPS = new Set(["textscan", "infotree", "profiler"]);
const RUNTIME_GROUPS = new Set(["cpu", "gpu", "memory", "ffi"]);
const categoryToggle = document.getElementById("categoryToggle");
const urlParams = new URLSearchParams(window.location.search);
const initialDataUrl = urlParams.get("data");
const initialRoot = urlParams.get("root");
const initialSize = urlParams.get("size");
const initialColor = urlParams.get("color");
const initialList = urlParams.get("list");
const initialListCount = urlParams.get("listCount");
const state = {
    data: null,
    sizeKey: null,
    colorKey: null,
    listKey: null,
    rootPrefix: "",
    metricSpecs: [],
    currentView: "treemap",
    blendWeights: {},
    blendScores: null,
    metricCategory: "all",
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
function formatValue(value, kind) {
    if (!Number.isFinite(value))
        return "0";
    // Format based on metric kind
    if (kind === "time_ns") {
        // Nanoseconds → human-readable time
        if (value >= 1e9)
            return `${(value / 1e9).toFixed(2)}s`;
        if (value >= 1e6)
            return `${(value / 1e6).toFixed(2)}ms`;
        if (value >= 1e3)
            return `${(value / 1e3).toFixed(2)}µs`;
        return `${value.toFixed(0)}ns`;
    }
    if (kind === "bytes") {
        // Bytes → human-readable size
        if (value >= 1e9)
            return `${(value / 1e9).toFixed(2)}GB`;
        if (value >= 1e6)
            return `${(value / 1e6).toFixed(2)}MB`;
        if (value >= 1e3)
            return `${(value / 1e3).toFixed(2)}KB`;
        return `${value.toFixed(0)}B`;
    }
    if (kind === "ratio_bp" || kind === "rate_bp") {
        // Basis points → percentage
        return `${(value / 100).toFixed(2)}%`;
    }
    if (kind === "ratio") {
        // Plain ratio → percentage
        return `${(value * 100).toFixed(2)}%`;
    }
    // Default formatting
    if (Math.abs(value) >= 1000)
        return value.toLocaleString();
    return Number.isInteger(value) ? value.toString() : value.toFixed(3);
}
/** Get the kind of a metric by key. */
function getMetricKind(key) {
    if (!key)
        return undefined;
    const spec = state.metricSpecs.find((s) => s.key === key);
    return spec?.kind;
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
/** Check if a metric group passes the current category filter. */
function groupPassesCategoryFilter(group, category) {
    if (category === "all")
        return true;
    if (category === "compile")
        return COMPILE_GROUPS.has(group) || group === "other";
    if (category === "runtime")
        return RUNTIME_GROUPS.has(group) || group === "other";
    return true;
}
/** Group specs by their group property and sort by group order. */
function groupSpecsByCategory(specs, category = "all") {
    const grouped = new Map();
    specs.forEach((spec) => {
        const group = spec.group || "other";
        // Filter by category
        if (!groupPassesCategoryFilter(group, category))
            return;
        if (!grouped.has(group))
            grouped.set(group, []);
        grouped.get(group).push(spec);
    });
    // Sort groups by their defined order
    const sortedGroups = new Map();
    const sortedKeys = Array.from(grouped.keys()).sort((a, b) => {
        const orderA = METRIC_GROUPS[a]?.order ?? 99;
        const orderB = METRIC_GROUPS[b]?.order ?? 99;
        return orderA - orderB;
    });
    sortedKeys.forEach((key) => {
        sortedGroups.set(key, grouped.get(key));
    });
    return sortedGroups;
}
/** Populate a select element with grouped options. */
function populateSelectWithGroups(select, grouped, includeBlend = false) {
    select.innerHTML = "";
    if (includeBlend) {
        const blendOpt = document.createElement("option");
        blendOpt.value = BLEND_KEY;
        blendOpt.textContent = "⚡ Blend (weighted)";
        select.appendChild(blendOpt);
    }
    grouped.forEach((specs, groupKey) => {
        const groupLabel = METRIC_GROUPS[groupKey]?.label || groupKey;
        const optgroup = document.createElement("optgroup");
        optgroup.label = groupLabel;
        specs.forEach((spec) => {
            const opt = document.createElement("option");
            opt.value = spec.key;
            opt.textContent = spec.label || spec.key;
            optgroup.appendChild(opt);
        });
        select.appendChild(optgroup);
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
    // Group specs by category (applies filter)
    const grouped = groupSpecsByCategory(specs, state.metricCategory);
    // Get keys from filtered groups only
    const filteredKeys = [];
    grouped.forEach((groupSpecs) => {
        groupSpecs.forEach((spec) => filteredKeys.push(spec.key));
    });
    // Populate selects with grouped options
    populateSelectWithGroups(sizeSelect, grouped, false);
    populateSelectWithGroups(colorSelect, grouped, true);
    if (listSelect)
        populateSelectWithGroups(listSelect, grouped, true);
    // Set default values (use filtered keys)
    const keys = filteredKeys;
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
function applyMetricOverrides() {
    if (!sizeSelect || !colorSelect)
        return;
    if (initialSize)
        state.sizeKey = initialSize;
    if (initialColor)
        state.colorKey = initialColor;
    if (initialList)
        state.listKey = initialList;
    if (state.sizeKey)
        sizeSelect.value = state.sizeKey;
    if (state.colorKey)
        colorSelect.value = state.colorKey;
    if (listSelect && state.listKey)
        listSelect.value = state.listKey;
    if (listCount && initialListCount)
        listCount.value = initialListCount;
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
        const listKind = state.listKey === BLEND_KEY ? undefined : getMetricKind(state.listKey);
        value.textContent = formatValue(listMetricValue(leaf.data, leaf), listKind);
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
        const kind = getMetricKind(key);
        val.textContent = formatValue(value, kind);
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
    const sizeKind = getMetricKind(state.sizeKey);
    const colorVal = state.colorKey === BLEND_KEY ? state.blendScores?.get(d) || 0 : metricValue(d.data, state.colorKey);
    const colorKind = state.colorKey === BLEND_KEY ? undefined : getMetricKind(state.colorKey);
    tooltip.innerHTML = `
    <div><strong>${name}</strong></div>
    <div>${path}</div>
    <div>size: ${labelForKey(state.sizeKey)} = ${formatValue(sizeVal, sizeKind)}</div>
    <div>color: ${labelForKey(state.colorKey)} = ${formatValue(colorVal, colorKind)}</div>
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
async function loadFromUrl(url) {
    const resolved = new URL(url, window.location.href).toString();
    const resp = await fetch(resolved);
    const parsed = (await resp.json());
    state.data = parsed;
    if (!state.rootPrefix && parsed.project?.root && parsed.project.root.startsWith("/")) {
        state.rootPrefix = parsed.project.root;
        if (rootInput)
            rootInput.value = state.rootPrefix;
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
// Category toggle
if (categoryToggle) {
    const categoryButtons = categoryToggle.querySelectorAll(".category-btn");
    categoryButtons.forEach((btn) => {
        btn.addEventListener("click", () => {
            const category = btn.dataset.category;
            if (category && category !== state.metricCategory) {
                state.metricCategory = category;
                // Update button active states
                categoryButtons.forEach((b) => {
                    b.classList.toggle("active", b.dataset.category === category);
                });
                // Refresh selectors with filtered metrics
                updateMetricSelectors();
                renderTreemap();
            }
        });
    });
}
window.addEventListener("resize", () => {
    if (state.data)
        renderCurrentView();
});
// ==================== View Switching ====================
function switchView(view) {
    state.currentView = view;
    // Update tab active state
    viewTabs.forEach((tab) => {
        const tabView = tab.dataset.view;
        tab.classList.toggle("active", tabView === view);
    });
    // Show/hide view containers
    if (treemapView)
        treemapView.hidden = view !== "treemap";
    if (timelineView)
        timelineView.hidden = view !== "timeline";
    if (allocsView)
        allocsView.hidden = view !== "allocs";
    renderCurrentView();
}
function renderCurrentView() {
    switch (state.currentView) {
        case "treemap":
            renderTreemap();
            break;
        case "timeline":
            renderTimeline();
            break;
        case "allocs":
            renderAllocs();
            break;
    }
}
// Set up tab click handlers
viewTabs.forEach((tab) => {
    tab.addEventListener("click", () => {
        const view = tab.dataset.view;
        if (view)
            switchView(view);
    });
});
// ==================== Timeline View (GPU Gantt Chart) ====================
function renderTimeline() {
    if (!state.data || !timelineEl)
        return;
    timelineEl.innerHTML = "";
    const width = timelineEl.clientWidth;
    const height = timelineEl.clientHeight || 500;
    const margin = { top: 40, right: 30, bottom: 50, left: 150 };
    // Get GPU kernels from data
    const kernels = state.data.gpu_kernels || [];
    if (kernels.length === 0) {
        timelineEl.innerHTML = `
      <div style="display: flex; align-items: center; justify-content: center; height: 100%; color: var(--muted);">
        <p>No GPU kernel data available. Import a trace with --metal, --gpu-json, or include gpu_kernels in your JSON.</p>
      </div>
    `;
        return;
    }
    // Calculate time range
    const minStart = Math.min(...kernels.map((k) => k.start_ns || 0));
    const maxEnd = Math.max(...kernels.map((k) => (k.start_ns || 0) + (k.duration_ns || 0)));
    const timeRange = maxEnd - minStart || 1;
    // Get unique kernel names for Y axis
    const kernelNames = [...new Set(kernels.map((k) => k.name))];
    // Create scales
    const xScale = d3
        .scaleLinear()
        .domain([0, timeRange / 1e6]) // Convert to ms
        .range([margin.left, width - margin.right]);
    const yScale = d3
        .scaleBand()
        .domain(kernelNames)
        .range([margin.top, height - margin.bottom])
        .padding(0.2);
    const colorScale = d3.scaleOrdinal(d3.schemeTableau10).domain(kernelNames);
    // Create SVG
    const svg = d3
        .select(timelineEl)
        .append("svg")
        .attr("width", width)
        .attr("height", height);
    // Add X axis
    svg
        .append("g")
        .attr("transform", `translate(0,${height - margin.bottom})`)
        .call(d3.axisBottom(xScale).ticks(10))
        .append("text")
        .attr("x", width / 2)
        .attr("y", 35)
        .attr("fill", "currentColor")
        .attr("text-anchor", "middle")
        .text("Time (ms)");
    // Add Y axis
    svg
        .append("g")
        .attr("transform", `translate(${margin.left},0)`)
        .call(d3.axisLeft(yScale))
        .selectAll("text")
        .style("font-size", "11px");
    // Add kernel bars
    svg
        .selectAll(".kernel-bar")
        .data(kernels)
        .enter()
        .append("rect")
        .attr("class", "kernel-bar")
        .attr("x", (d) => xScale(((d.start_ns || 0) - minStart) / 1e6))
        .attr("y", (d) => yScale(d.name) || 0)
        .attr("width", (d) => Math.max(2, xScale((d.duration_ns || 0) / 1e6) - margin.left))
        .attr("height", yScale.bandwidth())
        .attr("fill", (d) => colorScale(d.name))
        .on("mousemove", (event, d) => showKernelTooltip(event, d))
        .on("mouseleave", hideTooltip);
}
function showKernelTooltip(event, kernel) {
    if (!tooltip)
        return;
    const durationMs = ((kernel.duration_ns || 0) / 1e6).toFixed(3);
    tooltip.innerHTML = `
    <div><strong>${kernel.name}</strong></div>
    <div>Duration: ${durationMs} ms</div>
    ${kernel.occupancy_percent ? `<div>Occupancy: ${kernel.occupancy_percent.toFixed(1)}%</div>` : ""}
    ${kernel.bandwidth_gbps ? `<div>Bandwidth: ${kernel.bandwidth_gbps.toFixed(1)} GB/s</div>` : ""}
    ${kernel.lean_decl ? `<div>Decl: ${kernel.lean_decl}</div>` : ""}
  `;
    tooltip.hidden = false;
    tooltip.style.left = `${event.clientX + 12}px`;
    tooltip.style.top = `${event.clientY + 12}px`;
}
// ==================== Allocs View (Memory Waterfall) ====================
function renderAllocs() {
    if (!state.data || !allocsEl)
        return;
    allocsEl.innerHTML = "";
    const width = allocsEl.clientWidth;
    const height = allocsEl.clientHeight || 500;
    const margin = { top: 40, right: 30, bottom: 50, left: 80 };
    // Get memory events from data
    const events = state.data.memory_events || [];
    if (events.length === 0) {
        allocsEl.innerHTML = `
      <div style="display: flex; align-items: center; justify-content: center; height: 100%; color: var(--muted);">
        <p>No memory event data available. Import a trace with memory_events in your JSON.</p>
      </div>
    `;
        return;
    }
    // Calculate running balance over time
    let balance = 0;
    const timeline = [];
    events.forEach((e, idx) => {
        balance += e.type === "alloc" ? e.bytes : -e.bytes;
        timeline.push({ time: e.timestamp_ns || idx, balance, event: e });
    });
    // Create scales
    const xScale = d3
        .scaleLinear()
        .domain([0, timeline.length - 1])
        .range([margin.left, width - margin.right]);
    const maxBalance = Math.max(...timeline.map((t) => Math.abs(t.balance)));
    const yScale = d3
        .scaleLinear()
        .domain([0, maxBalance])
        .range([height - margin.bottom, margin.top]);
    // Create SVG
    const svg = d3
        .select(allocsEl)
        .append("svg")
        .attr("width", width)
        .attr("height", height);
    // Add X axis
    svg
        .append("g")
        .attr("transform", `translate(0,${height - margin.bottom})`)
        .call(d3.axisBottom(xScale).ticks(10))
        .append("text")
        .attr("x", width / 2)
        .attr("y", 35)
        .attr("fill", "currentColor")
        .attr("text-anchor", "middle")
        .text("Event Index");
    // Add Y axis
    svg
        .append("g")
        .attr("transform", `translate(${margin.left},0)`)
        .call(d3.axisLeft(yScale).tickFormat((d) => formatValue(d, "bytes")))
        .append("text")
        .attr("transform", "rotate(-90)")
        .attr("x", -height / 2)
        .attr("y", -60)
        .attr("fill", "currentColor")
        .attr("text-anchor", "middle")
        .text("Memory Balance");
    // Add area fill
    const area = d3
        .area()
        .x((_, i) => xScale(i))
        .y0(yScale(0))
        .y1((d) => yScale(Math.abs(d.balance)));
    svg
        .append("path")
        .datum(timeline)
        .attr("class", "alloc-area")
        .attr("fill", "#4bd5ff")
        .attr("d", area);
    // Add line
    const line = d3
        .line()
        .x((_, i) => xScale(i))
        .y((d) => yScale(Math.abs(d.balance)));
    svg
        .append("path")
        .datum(timeline)
        .attr("class", "alloc-line")
        .attr("stroke", "#4bd5ff")
        .attr("d", line);
    // Add event markers
    svg
        .selectAll(".alloc-event")
        .data(timeline)
        .enter()
        .append("circle")
        .attr("class", (d) => `alloc-event ${d.event.type}`)
        .attr("cx", (_, i) => xScale(i))
        .attr("cy", (d) => yScale(Math.abs(d.balance)))
        .attr("r", 4)
        .on("mousemove", (event, d) => showAllocTooltip(event, d))
        .on("mouseleave", hideTooltip);
}
function showAllocTooltip(event, data) {
    if (!tooltip)
        return;
    const e = data.event;
    tooltip.innerHTML = `
    <div><strong>${e.type === "alloc" ? "Allocation" : "Free"}</strong></div>
    <div>Size: ${formatValue(e.bytes, "bytes")}</div>
    <div>Balance: ${formatValue(data.balance, "bytes")}</div>
    ${e.lean_decl ? `<div>Decl: ${e.lean_decl}</div>` : ""}
    ${e.file ? `<div>File: ${e.file}</div>` : ""}
  `;
    tooltip.hidden = false;
    tooltip.style.left = `${event.clientX + 12}px`;
    tooltip.style.top = `${event.clientY + 12}px`;
}
// Set up Timeline/Allocs control event handlers
timelineColorBy?.addEventListener("change", () => renderTimeline());
allocsGroupBy?.addEventListener("change", () => renderAllocs());
