import http from "http";
import { readFile, stat } from "fs/promises";
import { createReadStream } from "fs";
import { extname, join, normalize } from "path";

const args = process.argv.slice(2);
const rootIdx = args.indexOf("--root");
const portIdx = args.indexOf("--port");
const root = rootIdx >= 0 ? args[rootIdx + 1] : process.cwd();
const port = portIdx >= 0 ? Number(args[portIdx + 1]) : 8765;

const mime = {
  ".html": "text/html; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".js": "application/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".svg": "image/svg+xml",
  ".png": "image/png",
};

function safePath(urlPath) {
  const cleaned = urlPath.split("?")[0].replace(/^\/+/, "");
  const full = normalize(join(root, cleaned));
  if (!full.startsWith(normalize(root))) return null;
  return full;
}

const server = http.createServer(async (req, res) => {
  const url = req.url || "/";
  const path = url === "/" ? "visualizer/index.html" : url;
  const full = safePath(path);
  if (!full) {
    res.writeHead(403);
    res.end("Forbidden");
    return;
  }

  try {
    const st = await stat(full);
    if (st.isDirectory()) {
      res.writeHead(302, { Location: "/visualizer/index.html" });
      res.end();
      return;
    }
    const ext = extname(full).toLowerCase();
    const contentType = mime[ext] || "application/octet-stream";
    res.writeHead(200, { "Content-Type": contentType });
    createReadStream(full).pipe(res);
  } catch {
    res.writeHead(404);
    res.end("Not found");
  }
});

server.listen(port, () => {
  console.log(`serving ${root} at http://localhost:${port}`);
});
