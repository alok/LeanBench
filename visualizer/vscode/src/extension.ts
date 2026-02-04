import * as vscode from "vscode";
import * as fs from "fs";
import * as path from "path";

type SpanMetrics = Record<string, number>;

type SpanEntry = {
  id: string;
  name: string;
  kind: string;
  path: string;
  span: {
    line: number;
    col: number;
    end_line: number;
    end_col: number;
  };
  metrics: SpanMetrics;
};

type SpansFile = {
  root: string;
  spans: SpanEntry[];
};

type MetricMeta = {
  key: string;
  label?: string;
  unit?: string;
  group?: string;
  description?: string;
  kind?: string;
  default_blend_weight?: number;
};

type MetricStats = {
  min: number;
  max: number;
};

type SpanScore = {
  span: SpanEntry;
  rawScore: number;
  score: number;
  range: vscode.Range;
  absPath: string;
  contributions: Array<{ key: string; value: number; normalized: number; weight: number; contribution: number }>;
};

type ExtensionState = {
  spansFile?: SpansFile;
  spansPath?: string;
  metricsMeta?: Map<string, MetricMeta>;
  metricsPath?: string;
  metricKeys: Set<string>;
  scores: SpanScore[];
  byFile: Map<string, SpanScore[]>;
  decorations: vscode.TextEditorDecorationType[];
  paletteKey?: string;
  statusBar?: vscode.StatusBarItem;
  legendPanel?: vscode.WebviewPanel;
  watcher?: vscode.FileSystemWatcher;
  output: vscode.OutputChannel;
};

const CONFIG_SECTION = "leanObservatory";
const DECORATION_BUCKETS = 20;
const PALETTE_KEYS = ["heat", "blueOrange", "viridis"] as const;
type PaletteKey = (typeof PALETTE_KEYS)[number];

const PALETTES: Record<PaletteKey, Array<{ t: number; color: [number, number, number] }>> = {
  heat: [
    { t: 0.0, color: [10, 25, 70] },
    { t: 0.25, color: [40, 120, 200] },
    { t: 0.5, color: [90, 220, 180] },
    { t: 0.7, color: [240, 220, 70] },
    { t: 0.85, color: [245, 120, 60] },
    { t: 1.0, color: [200, 40, 40] },
  ],
  blueOrange: [
    { t: 0.0, color: [30, 80, 200] },
    { t: 0.5, color: [240, 240, 240] },
    { t: 1.0, color: [240, 140, 40] },
  ],
  viridis: [
    { t: 0.0, color: [68, 1, 84] },
    { t: 0.25, color: [59, 82, 139] },
    { t: 0.5, color: [33, 145, 140] },
    { t: 0.75, color: [94, 201, 98] },
    { t: 1.0, color: [253, 231, 37] },
  ],
};

let state: ExtensionState | null = null;

export function activate(context: vscode.ExtensionContext): void {
  const output = vscode.window.createOutputChannel("Lean Observatory");
  state = {
    metricKeys: new Set<string>(),
    scores: [],
    byFile: new Map(),
    decorations: [],
    output,
  };

  context.subscriptions.push(output);
  context.subscriptions.push(
    vscode.commands.registerCommand("leanObservatory.loadSpans", async () => {
      await loadSpansCommand(false);
    })
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("leanObservatory.reload", async () => {
      await reloadFromConfig(false);
    })
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("leanObservatory.clear", async () => {
      clearDecorations();
      state?.output.appendLine("Cleared heatmap decorations.");
    })
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("leanObservatory.editWeights", async () => {
      await editWeightsCommand();
    })
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("leanObservatory.showLegend", async () => {
      await showLegend();
    })
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("leanObservatory.cyclePalette", async () => {
      await cyclePalette();
    })
  );

  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration(async (e) => {
      const affects = (key: string) => e.affectsConfiguration(`${CONFIG_SECTION}.${key}`);
      if (affects("spansJsonPath") || affects("metricsJsonPath")) {
        initStatusBar();
        await reloadFromConfig(true);
        return;
      }
      if (
        affects("weights") ||
        affects("normalize") ||
        affects("includeKinds") ||
        affects("minScore") ||
        affects("maxDecorationsPerFile") ||
        affects("showHover")
      ) {
        rebuildScores();
        refreshVisibleEditors();
      }
      if (affects("palette") || affects("showLegend")) {
        initStatusBar();
        updateStatusBar();
        updateLegend();
        refreshVisibleEditors();
      }
    })
  );

  context.subscriptions.push(
    vscode.window.onDidChangeVisibleTextEditors(() => {
      refreshVisibleEditors();
    })
  );
  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor(() => {
      refreshVisibleEditors();
    })
  );

  initStatusBar();
  void reloadFromConfig(true);
}

export function deactivate(): void {
  clearDecorations();
  disposeWatcher();
  disposeStatusBar();
  disposeLegend();
}

async function loadSpansCommand(silent: boolean): Promise<void> {
  const cfgPath = getConfigPath();
  if (cfgPath) {
    await loadSpansFromPath(cfgPath, silent);
    return;
  }

  const picked = await vscode.window.showOpenDialog({
    canSelectMany: false,
    openLabel: "Open spans.json",
    filters: { "JSON": ["json"] },
  });
  if (!picked || picked.length === 0) {
    if (!silent) {
      vscode.window.showWarningMessage("Lean Observatory: No spans.json selected.");
    }
    return;
  }
  await loadSpansFromPath(picked[0].fsPath, silent);
}

async function reloadFromConfig(silent: boolean): Promise<void> {
  const cfgPath = getConfigPath();
  if (!cfgPath) {
    if (!silent) {
      vscode.window.showInformationMessage("Lean Observatory: Configure leanObservatory.spansJsonPath or use the Load command.");
    }
    return;
  }
  await loadSpansFromPath(cfgPath, silent);
}

function getConfigPath(): string | null {
  const cfg = vscode.workspace.getConfiguration(CONFIG_SECTION);
  const spansPath = cfg.get<string>("spansJsonPath", "").trim();
  if (!spansPath) {
    return null;
  }
  const abs = resolvePath(spansPath);
  return abs;
}

function resolvePath(p: string): string {
  if (path.isAbsolute(p)) {
    return p;
  }
  const root = workspaceRoot();
  if (!root) {
    return path.resolve(p);
  }
  return path.join(root, p);
}

function workspaceRoot(): string | null {
  const folders = vscode.workspace.workspaceFolders;
  if (!folders || folders.length === 0) {
    return null;
  }
  return folders[0].uri.fsPath;
}

function collectMetricKeys(spans: SpanEntry[]): Set<string> {
  const keys = new Set<string>();
  for (const span of spans) {
    for (const key of Object.keys(span.metrics ?? {})) {
      keys.add(key);
    }
  }
  return keys;
}

async function loadMetricsMetadata(spansPath: string): Promise<void> {
  if (!state) return;
  const cfg = vscode.workspace.getConfiguration(CONFIG_SECTION);
  const configured = cfg.get<string>("metricsJsonPath", "").trim();
  const metricsPath = inferMetricsPath(spansPath, configured);
  if (!metricsPath) {
    state.metricsMeta = undefined;
    state.metricsPath = undefined;
    return;
  }
  try {
    const raw = await fs.promises.readFile(metricsPath, "utf8");
    const parsed = JSON.parse(raw) as { metrics?: MetricMeta[] };
    if (!parsed || !Array.isArray(parsed.metrics)) {
      throw new Error("Invalid metrics.json shape.");
    }
    const meta = new Map<string, MetricMeta>();
    for (const entry of parsed.metrics) {
      if (entry && entry.key) {
        meta.set(entry.key, entry);
      }
    }
    state.metricsMeta = meta;
    state.metricsPath = metricsPath;
    state.output.appendLine(`Loaded metrics metadata: ${meta.size} entries from ${metricsPath}`);
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    state.output.appendLine(`Failed to load metrics metadata: ${msg}`);
    state.metricsMeta = undefined;
    state.metricsPath = undefined;
  }
}

function inferMetricsPath(spansPath: string, configured: string): string | null {
  if (configured) {
    return resolvePath(configured);
  }
  const dir = path.dirname(spansPath);
  const candidate = path.join(dir, "metrics.json");
  if (fs.existsSync(candidate)) {
    return candidate;
  }
  return null;
}

async function loadSpansFromPath(spansPath: string, silent: boolean): Promise<void> {
  if (!state) {
    return;
  }
  try {
    const raw = await fs.promises.readFile(spansPath, "utf8");
    const parsed = JSON.parse(raw) as SpansFile;
    if (!parsed || !Array.isArray(parsed.spans)) {
      throw new Error("Invalid spans.json shape.");
    }
    state.spansFile = parsed;
    state.spansPath = spansPath;
    state.metricKeys = collectMetricKeys(parsed.spans);
    await loadMetricsMetadata(spansPath);
    state.output.appendLine(`Loaded spans: ${parsed.spans.length} nodes from ${spansPath}`);
    rebuildScores();
    refreshVisibleEditors();
    ensureWatcher(spansPath);
    if (!silent) {
      vscode.window.showInformationMessage(`Lean Observatory: Loaded ${parsed.spans.length} spans.`);
    }
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err);
    state.output.appendLine(`Failed to load spans.json: ${msg}`);
    if (!silent) {
      vscode.window.showErrorMessage(`Lean Observatory: Failed to load spans.json (${msg}).`);
    }
  }
}

function ensureWatcher(spansPath: string): void {
  if (!state) {
    return;
  }
  if (state.watcher) {
    const current = state.spansPath;
    if (current === spansPath) {
      return;
    }
    disposeWatcher();
  }
  const dir = path.dirname(spansPath);
  const base = path.basename(spansPath);
  const pattern = new vscode.RelativePattern(dir, base);
  const watcher = vscode.workspace.createFileSystemWatcher(pattern);
  watcher.onDidChange(() => void reloadFromConfig(true));
  watcher.onDidCreate(() => void reloadFromConfig(true));
  watcher.onDidDelete(() => clearDecorations());
  state.watcher = watcher;
}

function disposeWatcher(): void {
  if (state?.watcher) {
    state.watcher.dispose();
    state.watcher = undefined;
  }
}

function rebuildScores(): void {
  if (!state || !state.spansFile) {
    return;
  }
  const cfg = vscode.workspace.getConfiguration(CONFIG_SECTION);
  const weights = (cfg.get<Record<string, number>>("weights", {}) ?? {}) as Record<string, number>;
  const normalizeMode = cfg.get<string>("normalize", "max");
  const includeKinds = new Set(cfg.get<string[]>("includeKinds", ["decl", "command"]) ?? []);
  const minScore = cfg.get<number>("minScore", 0);
  const maxPerFile = cfg.get<number>("maxDecorationsPerFile", 2000);

  const weightKeys = Object.keys(weights).filter((k) => Number.isFinite(weights[k]) && weights[k] !== 0);
  if (weightKeys.length === 0) {
    state.output.appendLine("No weights configured; skipping heatmap.");
    state.scores = [];
    state.byFile = new Map();
    clearDecorations();
    return;
  }

  const rootPath = resolveRoot(state.spansFile.root);
  if (!rootPath) {
    state.output.appendLine("Cannot resolve spans root path; skipping.");
    return;
  }

  const stats = computeStats(state.spansFile.spans, weightKeys, includeKinds);
  const rawScores: SpanScore[] = [];

  for (const span of state.spansFile.spans) {
    if (!includeKinds.has(span.kind)) {
      continue;
    }
    const absPath = resolveSpanPath(rootPath, span.path);
    if (!absPath) {
      continue;
    }
    const { rawScore, contributions } = scoreSpan(span, weightKeys, weights, stats, normalizeMode);
    if (rawScore === null) {
      continue;
    }
    const range = toRange(span.span);
    if (!range) {
      continue;
    }
    rawScores.push({ span, rawScore, score: rawScore, range, absPath, contributions });
  }

  let min = Infinity;
  let max = -Infinity;
  for (const entry of rawScores) {
    if (entry.rawScore < min) min = entry.rawScore;
    if (entry.rawScore > max) max = entry.rawScore;
  }
  if (!Number.isFinite(min) || !Number.isFinite(max)) {
    min = 0;
    max = 1;
  }
  const denom = max - min;

  for (const entry of rawScores) {
    const normalized = denom === 0 ? 0 : (entry.rawScore - min) / denom;
    entry.score = normalized;
  }

  const byFile = new Map<string, SpanScore[]>();
  for (const entry of rawScores) {
    if (entry.score < minScore) {
      continue;
    }
    const file = path.normalize(entry.absPath);
    const bucket = byFile.get(file) ?? [];
    bucket.push(entry);
    byFile.set(file, bucket);
  }

  for (const [file, entries] of byFile) {
    if (entries.length > maxPerFile) {
      entries.sort((a, b) => b.score - a.score);
      byFile.set(file, entries.slice(0, maxPerFile));
    }
  }

  state.scores = rawScores;
  state.byFile = byFile;
  state.output.appendLine(`Prepared ${rawScores.length} scored spans across ${byFile.size} files.`);
  updateLegend();
}

function computeStats(spans: SpanEntry[], keys: string[], includeKinds: Set<string>): Map<string, MetricStats> {
  const stats = new Map<string, MetricStats>();
  for (const key of keys) {
    stats.set(key, { min: Infinity, max: -Infinity });
  }
  for (const span of spans) {
    if (!includeKinds.has(span.kind)) {
      continue;
    }
    for (const key of keys) {
      const value = span.metrics?.[key];
      if (!Number.isFinite(value)) {
        continue;
      }
      const stat = stats.get(key);
      if (!stat) continue;
      if (value < stat.min) stat.min = value;
      if (value > stat.max) stat.max = value;
    }
  }
  for (const [key, stat] of stats) {
    if (!Number.isFinite(stat.min)) stat.min = 0;
    if (!Number.isFinite(stat.max)) stat.max = 0;
    stats.set(key, stat);
  }
  return stats;
}

function scoreSpan(
  span: SpanEntry,
  keys: string[],
  weights: Record<string, number>,
  stats: Map<string, MetricStats>,
  normalizeMode: string
): { rawScore: number | null; contributions: SpanScore["contributions"] } {
  let raw = 0;
  let has = false;
  const contributions: SpanScore["contributions"] = [];

  for (const key of keys) {
    const weight = weights[key];
    const value = span.metrics?.[key];
    if (!Number.isFinite(value)) {
      continue;
    }
    const stat = stats.get(key);
    if (!stat) continue;
    const normalized = normalizeMetric(value, stat, normalizeMode);
    const contribution = normalized * weight;
    raw += contribution;
    has = true;
    contributions.push({ key, value, normalized, weight, contribution });
  }

  return { rawScore: has ? raw : null, contributions };
}

function normalizeMetric(value: number, stat: MetricStats, mode: string): number {
  if (mode === "minmax") {
    const denom = stat.max - stat.min;
    if (denom === 0) return 0;
    return (value - stat.min) / denom;
  }
  if (stat.max === 0) {
    return 0;
  }
  return value / stat.max;
}

function resolveRoot(root: string): string | null {
  if (path.isAbsolute(root)) {
    return root;
  }
  const workspace = workspaceRoot();
  if (!workspace) {
    return null;
  }
  return path.join(workspace, root);
}

function resolveSpanPath(root: string, relPath: string): string | null {
  if (!relPath) {
    return null;
  }
  const parts = relPath.split("/").filter(Boolean);
  return path.join(root, ...parts);
}

function toRange(span: SpanEntry["span"]): vscode.Range | null {
  const startLine = Math.max(span.line - 1, 0);
  const startCol = Math.max(span.col - 1, 0);
  const endLine = Math.max(span.end_line - 1, 0);
  const endCol = Math.max(span.end_col - 1, 0);
  if (endLine < startLine) {
    return null;
  }
  const start = new vscode.Position(startLine, startCol);
  const end = new vscode.Position(endLine, endCol);
  return new vscode.Range(start, end);
}

function refreshVisibleEditors(): void {
  if (!state) {
    return;
  }
  if (!state.scores.length) {
    clearDecorations();
    return;
  }
  ensureDecorations();
  const editors = vscode.window.visibleTextEditors;
  for (const editor of editors) {
    applyDecorations(editor);
  }
}

function ensureDecorations(): void {
  if (!state) return;
  const palette = currentPalette();
  if (state.decorations.length === DECORATION_BUCKETS && state.paletteKey === palette) {
    return;
  }
  clearDecorations();
  const decos: vscode.TextEditorDecorationType[] = [];
  const bucketCount = DECORATION_BUCKETS as number;
  for (let i = 0; i < bucketCount; i += 1) {
    const t = bucketCount === 1 ? 0 : i / (bucketCount - 1);
    const color = colorFor(t, 0.35, palette);
    const border = colorFor(t, 0.6, palette);
    decos.push(
      vscode.window.createTextEditorDecorationType({
        backgroundColor: color,
        border: `1px solid ${border}`,
        overviewRulerColor: color,
        overviewRulerLane: vscode.OverviewRulerLane.Right,
        isWholeLine: false,
      })
    );
  }
  state.decorations = decos;
  state.paletteKey = palette;
}

function clearDecorations(): void {
  if (!state) return;
  for (const deco of state.decorations) {
    deco.dispose();
  }
  state.decorations = [];
}

function applyDecorations(editor: vscode.TextEditor): void {
  if (!state) return;
  const filePath = path.normalize(editor.document.uri.fsPath);
  const spans = state.byFile.get(filePath);
  if (!spans || spans.length === 0) {
    for (const deco of state.decorations) {
      editor.setDecorations(deco, []);
    }
    return;
  }

  const showHover = vscode.workspace.getConfiguration(CONFIG_SECTION).get<boolean>("showHover", true);
  const buckets: vscode.DecorationOptions[][] = Array.from({ length: DECORATION_BUCKETS }, () => []);

  for (const entry of spans) {
    const bucketIdx = bucketFor(entry.score);
    const hover = showHover ? makeHover(entry) : undefined;
    buckets[bucketIdx].push({ range: entry.range, hoverMessage: hover });
  }

  for (let i = 0; i < DECORATION_BUCKETS; i += 1) {
    editor.setDecorations(state.decorations[i], buckets[i]);
  }
}

function bucketFor(score: number): number {
  if (!Number.isFinite(score)) {
    return 0;
  }
  const clamped = Math.max(0, Math.min(1, score));
  const idx = Math.floor(clamped * (DECORATION_BUCKETS - 1));
  return Math.max(0, Math.min(DECORATION_BUCKETS - 1, idx));
}

function colorFor(t: number, alpha: number, palette: PaletteKey): string {
  const rgb = interpolatePalette(palette, t);
  const clampedAlpha = Math.max(0, Math.min(1, alpha));
  return `rgba(${rgb[0]}, ${rgb[1]}, ${rgb[2]}, ${clampedAlpha})`;
}

function interpolatePalette(palette: PaletteKey, t: number): [number, number, number] {
  const stops = PALETTES[palette] ?? PALETTES.heat;
  const clamped = Math.max(0, Math.min(1, t));
  let left = stops[0];
  let right = stops[stops.length - 1];
  for (let i = 0; i < stops.length - 1; i += 1) {
    if (clamped >= stops[i].t && clamped <= stops[i + 1].t) {
      left = stops[i];
      right = stops[i + 1];
      break;
    }
  }
  const span = right.t - left.t;
  const localT = span === 0 ? 0 : (clamped - left.t) / span;
  const r = Math.round(left.color[0] + (right.color[0] - left.color[0]) * localT);
  const g = Math.round(left.color[1] + (right.color[1] - left.color[1]) * localT);
  const b = Math.round(left.color[2] + (right.color[2] - left.color[2]) * localT);
  return [r, g, b];
}

function makeHover(entry: SpanScore): vscode.MarkdownString {
  const span = entry.span;
  const md = new vscode.MarkdownString();
  md.isTrusted = false;
  md.appendMarkdown(`**${span.name}**  \\n`);
  md.appendMarkdown(`Kind: ${span.kind}  \\n`);
  md.appendMarkdown(`Path: ${span.path}:${span.span.line}:${span.span.col}  \\n`);
  md.appendMarkdown(`Score: ${entry.score.toFixed(3)} (raw ${entry.rawScore.toFixed(3)})  \\n`);
  if (entry.contributions.length > 0) {
    md.appendMarkdown("\\n**Contributions**\\n");
    for (const c of entry.contributions) {
      const label = formatMetricLabel(c.key);
      md.appendMarkdown(
        `- ${label}: ${formatNumber(c.value)} (n=${c.normalized.toFixed(3)}, w=${c.weight}, c=${c.contribution.toFixed(3)})\\n`
      );
    }
  }
  return md;
}

function formatNumber(value: number): string {
  if (!Number.isFinite(value)) {
    return "NaN";
  }
  if (Math.abs(value) >= 1000) {
    return value.toFixed(0);
  }
  if (Math.abs(value) >= 10) {
    return value.toFixed(2);
  }
  return value.toFixed(3);
}

function formatMetricLabel(key: string): string {
  const meta = state?.metricsMeta?.get(key);
  if (!meta) {
    return key;
  }
  const label = meta.label?.trim() || key;
  const unit = meta.unit?.trim();
  if (unit) {
    return `${label} (${unit})`;
  }
  return label;
}

function currentPalette(): PaletteKey {
  const cfg = vscode.workspace.getConfiguration(CONFIG_SECTION);
  const palette = cfg.get<string>("palette", "heat") as PaletteKey;
  if (PALETTE_KEYS.includes(palette)) {
    return palette;
  }
  return "heat";
}

async function editWeightsCommand(): Promise<void> {
  if (!state) return;
  const cfg = vscode.workspace.getConfiguration(CONFIG_SECTION);
  const weights = (cfg.get<Record<string, number>>("weights", {}) ?? {}) as Record<string, number>;
  const keys = new Set<string>([...state.metricKeys]);
  for (const key of Object.keys(weights)) {
    keys.add(key);
  }
  const items: vscode.QuickPickItem[] = [];
  items.push({ label: "Add custom metric key...", description: "" });
  for (const key of Array.from(keys).sort()) {
    const label = formatMetricLabel(key);
    const weight = weights[key];
    const desc = Number.isFinite(weight) ? `weight=${weight}` : "";
    const detail = formatMetricDetail(key);
    items.push({ label, description: desc, detail });
  }
  const pick = await vscode.window.showQuickPick(items, {
    title: "Edit metric weight",
    matchOnDescription: true,
    matchOnDetail: true,
  });
  if (!pick) return;
  let key: string | undefined;
  if (pick.label === "Add custom metric key...") {
    key = await vscode.window.showInputBox({
      prompt: "Metric key",
      placeHolder: "profile_weight",
      value: "",
    });
    if (!key) return;
  } else {
    key = findMetricKeyByLabel(pick.label) ?? pick.label;
  }
  const existing = weights[key];
  const input = await vscode.window.showInputBox({
    prompt: `Set weight for ${key} (empty to remove)`,
    value: Number.isFinite(existing) ? String(existing) : "",
    validateInput: (value) => {
      if (value.trim() === "") return null;
      return Number.isFinite(Number(value)) ? null : "Enter a number or leave empty.";
    },
  });
  if (input === undefined) return;
  const trimmed = input.trim();
  if (trimmed === "") {
    delete weights[key];
  } else {
    weights[key] = Number(trimmed);
  }
  await cfg.update("weights", weights, vscode.ConfigurationTarget.Workspace);
  state.output.appendLine(`Updated weight for ${key}`);
  rebuildScores();
  refreshVisibleEditors();
}

function findMetricKeyByLabel(label: string): string | undefined {
  if (!state?.metricsMeta) return undefined;
  for (const [key, meta] of state.metricsMeta.entries()) {
    const metaLabel = formatMetricLabel(key);
    if (metaLabel === label) {
      return key;
    }
  }
  return undefined;
}

function formatMetricDetail(key: string): string | undefined {
  const meta = state?.metricsMeta?.get(key);
  if (!meta) return undefined;
  const parts: string[] = [];
  if (meta.group) parts.push(meta.group);
  if (meta.kind) parts.push(meta.kind);
  if (meta.description) parts.push(meta.description);
  if (parts.length === 0) return undefined;
  return parts.join(" • ");
}

async function cyclePalette(): Promise<void> {
  const cfg = vscode.workspace.getConfiguration(CONFIG_SECTION);
  const current = currentPalette();
  const idx = PALETTE_KEYS.indexOf(current);
  const next = PALETTE_KEYS[(idx + 1) % PALETTE_KEYS.length];
  await cfg.update("palette", next, vscode.ConfigurationTarget.Workspace);
  state?.output.appendLine(`Palette set to ${next}`);
  updateStatusBar();
  updateLegend();
  refreshVisibleEditors();
}

async function showLegend(): Promise<void> {
  if (!state) return;
  const palette = currentPalette();
  if (!state.legendPanel) {
    state.legendPanel = vscode.window.createWebviewPanel(
      "leanObservatoryLegend",
      "Lean Observatory: Heatmap Legend",
      vscode.ViewColumn.Beside,
      { enableScripts: false }
    );
    state.legendPanel.onDidDispose(() => {
      if (state) {
        state.legendPanel = undefined;
      }
    });
  }
  state.legendPanel.webview.html = renderLegendHtml(palette);
  state.legendPanel.reveal(vscode.ViewColumn.Beside);
}

function renderLegendHtml(palette: PaletteKey): string {
  const stops = PALETTES[palette];
  const gradient = stops
    .map((s) => {
      const c = `rgb(${s.color[0]}, ${s.color[1]}, ${s.color[2]})`;
      return `${c} ${Math.round(s.t * 100)}%`;
    })
    .join(", ");
  return `<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <style>
      body { background: #111822; color: #e2e8f0; font-family: -apple-system, BlinkMacSystemFont, Segoe UI, sans-serif; padding: 20px; }
      .title { font-size: 18px; font-weight: 600; margin-bottom: 8px; }
      .subtitle { color: #9fb1c7; margin-bottom: 16px; }
      .bar { height: 24px; border-radius: 8px; background: linear-gradient(90deg, ${gradient}); box-shadow: inset 0 0 0 1px rgba(255,255,255,0.1); }
      .labels { display: flex; justify-content: space-between; font-size: 12px; margin-top: 6px; color: #a8b3c7; }
      .note { margin-top: 14px; font-size: 12px; color: #8aa0b8; }
    </style>
  </head>
  <body>
    <div class="title">Heatmap Legend (${palette})</div>
    <div class="subtitle">Low → High score</div>
    <div class="bar"></div>
    <div class="labels">
      <span>0</span>
      <span>0.5</span>
      <span>1.0</span>
    </div>
    <div class="note">Matches the editor background/overview ruler palette.</div>
  </body>
</html>`;
}

function initStatusBar(): void {
  if (!state) return;
  const cfg = vscode.workspace.getConfiguration(CONFIG_SECTION);
  const show = cfg.get<boolean>("showLegend", true);
  if (!show) {
    disposeStatusBar();
    return;
  }
  if (!state.statusBar) {
    state.statusBar = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
    state.statusBar.command = "leanObservatory.showLegend";
    state.statusBar.tooltip = "Lean Observatory: Show heatmap legend";
    state.statusBar.show();
  }
  updateStatusBar();
}

function updateStatusBar(): void {
  if (!state?.statusBar) return;
  const palette = currentPalette();
  state.statusBar.text = `LeanObs: ${palette}`;
  state.statusBar.show();
}

function disposeStatusBar(): void {
  if (state?.statusBar) {
    state.statusBar.dispose();
    state.statusBar = undefined;
  }
}

function updateLegend(): void {
  if (!state?.legendPanel) return;
  state.legendPanel.webview.html = renderLegendHtml(currentPalette());
}

function disposeLegend(): void {
  if (state?.legendPanel) {
    state.legendPanel.dispose();
    state.legendPanel = undefined;
  }
}
