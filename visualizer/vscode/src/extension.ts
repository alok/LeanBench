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
  scores: SpanScore[];
  byFile: Map<string, SpanScore[]>;
  decorations: vscode.TextEditorDecorationType[];
  watcher?: vscode.FileSystemWatcher;
  output: vscode.OutputChannel;
};

const CONFIG_SECTION = "leanObservatory";
const DECORATION_BUCKETS = 20;

let state: ExtensionState | null = null;

export function activate(context: vscode.ExtensionContext): void {
  const output = vscode.window.createOutputChannel("Lean Observatory");
  state = {
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
    vscode.workspace.onDidChangeConfiguration(async (e) => {
      if (e.affectsConfiguration(CONFIG_SECTION)) {
        await reloadFromConfig(true);
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

  void reloadFromConfig(true);
}

export function deactivate(): void {
  clearDecorations();
  disposeWatcher();
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
  if (state.decorations.length === DECORATION_BUCKETS) {
    return;
  }
  clearDecorations();
  const decos: vscode.TextEditorDecorationType[] = [];
  const bucketCount = DECORATION_BUCKETS as number;
  for (let i = 0; i < bucketCount; i += 1) {
    const t = bucketCount === 1 ? 0 : i / (bucketCount - 1);
    const color = colorFor(t, 0.35);
    const border = colorFor(t, 0.6);
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

function colorFor(t: number, alpha: number): string {
  const hue = 220 - 220 * t;
  const sat = 75;
  const light = 55;
  const clampedAlpha = Math.max(0, Math.min(1, alpha));
  return `hsla(${hue}, ${sat}%, ${light}%, ${clampedAlpha})`;
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
      md.appendMarkdown(
        `- ${c.key}: ${formatNumber(c.value)} (n=${c.normalized.toFixed(3)}, w=${c.weight}, c=${c.contribution.toFixed(3)})\\n`
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
