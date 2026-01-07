import LeanBench.Runtime.Types
import LeanBench.Runtime.GenericCollector
import LeanBench.Runtime.PerfCollector

/-!
# CUDA/NSight Collector

Parses NVIDIA NSight Systems profiler output for GPU kernel metrics.

Usage:
1. Profile: `nsys profile --stats=true --output=report ./your_binary`
2. Export kernels: `nsys stats --report gpukernsum --format json report.nsys-rep > kernels.json`
3. Export memory: `nsys stats --report cudaapisum --format json report.nsys-rep > api.json`
4. Import: pass `--cuda kernels.json` to leanobserve (or use generic --gpu-json)

The NSight JSON format varies by report type. This collector handles:
- gpukernsum: GPU kernel execution summary
- cudaapisum: CUDA API call summary
- gpumemtimesum: GPU memory operation summary
-/

namespace LeanBench.Runtime

open Lean (Json)

/-- Attempt to demangle a C++ symbol name.
    This is a simplified demangler for common patterns. -/
def demangleCppSymbol (mangled : String) : Option String := Id.run do
  if !mangled.startsWith "_Z" then return none

  -- Very basic C++ demangling - just extract identifier
  let rest := (mangled.drop 2).toString
  -- Skip length prefix and extract name
  let mut i := 0
  let mut len := 0
  for c in rest.toList do
    if c.isDigit then
      len := len * 10 + (c.toNat - '0'.toNat)
      i := i + 1
    else
      break

  if len > 0 && i + len <= rest.length then
    let name := ((rest.drop i).take len).toString
    -- Check if it looks like a Lean name
    if strContains name "lean" || strContains name "Lean" then
      return demangleLeanSymbol name
    else
      return some name
  else
    return none

/-- Map CUDA kernel name to Lean declaration.
    CUDA kernels are often named with C++ mangling or explicit names. -/
def mapCudaKernelToLean (kernelName : String) : Option String :=
  -- Try common patterns
  if kernelName.startsWith "lean_" then
    demangleLeanSymbol kernelName
  else if kernelName.startsWith "_Z" then
    -- C++ mangled name - try to extract the base name
    demangleCppSymbol kernelName
  else if strContains kernelName "kernel" || strContains kernelName "Kernel" then
    -- Try stripping common suffixes
    let base := kernelName.replace "_kernel" "" |>.replace "Kernel" ""
    if base.length > 0 then demangleLeanSymbol ("lean_" ++ base) else none
  else
    none

/-- Parse NSight gpukernsum JSON report. -/
def parseNsightKernelSum (json : Json) : Array GpuKernel := Id.run do
  let mut kernels : Array GpuKernel := #[]

  -- NSight JSON structure: array of kernel records
  let records := json.getObjValAs? (Array Json) "data" |>.toOption
    |>.getD (json.getArr? |>.toOption |>.getD #[])

  for record in records do
    -- Extract kernel name
    let name := record.getObjValAs? String "Name" |>.toOption
      |>.orElse (fun _ => record.getObjValAs? String "name" |>.toOption)
      |>.getD "unknown_kernel"

    -- Try to map CUDA kernel to Lean declaration
    let leanDecl := mapCudaKernelToLean name

    -- Duration fields vary: "Time (%)", "Total Time (ns)", "Avg (ns)"
    let totalTimeNs := record.getObjValAs? Float "Total Time (ns)" |>.toOption
      |>.orElse (fun _ => record.getObjValAs? Float "total_time_ns" |>.toOption)
    let avgTimeNs := record.getObjValAs? Float "Avg (ns)" |>.toOption
      |>.orElse (fun _ => record.getObjValAs? Float "avg_ns" |>.toOption)

    -- Invocation count
    let instances := record.getObjValAs? Nat "Instances" |>.toOption
      |>.orElse (fun _ => record.getObjValAs? Nat "count" |>.toOption)

    -- Grid/block dimensions (if available)
    let gridX := record.getObjValAs? Nat "Grid X" |>.toOption
    let gridY := record.getObjValAs? Nat "Grid Y" |>.toOption
    let gridZ := record.getObjValAs? Nat "Grid Z" |>.toOption
    let gridSize := match gridX, gridY, gridZ with
      | some x, some y, some z => some #[x, y, z]
      | some x, some y, none => some #[x, y, 1]
      | some x, none, none => some #[x, 1, 1]
      | _, _, _ => none

    let blockX := record.getObjValAs? Nat "Block X" |>.toOption
    let blockY := record.getObjValAs? Nat "Block Y" |>.toOption
    let blockZ := record.getObjValAs? Nat "Block Z" |>.toOption
    let blockSize := match blockX, blockY, blockZ with
      | some x, some y, some z => some #[x, y, z]
      | some x, some y, none => some #[x, y, 1]
      | some x, none, none => some #[x, 1, 1]
      | _, _, _ => none

    -- Registers and shared memory
    let registers := record.getObjValAs? Nat "Registers Per Thread" |>.toOption
      |>.orElse (fun _ => record.getObjValAs? Nat "registers" |>.toOption)
    let sharedMem := record.getObjValAs? Nat "Static SMem (bytes)" |>.toOption
      |>.orElse (fun _ => record.getObjValAs? Nat "shared_memory_bytes" |>.toOption)

    -- Occupancy (if available)
    let occupancy := record.getObjValAs? Float "Achieved Occupancy" |>.toOption
      |>.orElse (fun _ => record.getObjValAs? Float "occupancy_percent" |>.toOption)

    -- Create kernel entry (one per unique kernel name)
    -- If we have instances, multiply by count for total time
    let duration := totalTimeNs.orElse (fun _ =>
      match avgTimeNs, instances with
      | some avg, some n => some (avg * n.toFloat)
      | some avg, none => some avg
      | none, _ => none)

    kernels := kernels.push {
      name := name
      leanDecl := leanDecl
      file := leanDecl.bind (fun d => symbolToFile name (some d))
      durationNs := duration
      gridSize := gridSize
      blockSize := blockSize
      registersPerThread := registers
      sharedMemoryBytes := sharedMem
      occupancyPercent := occupancy
    }

  return kernels

/-- Parse NSight cudaapisum JSON report for memory operations. -/
def parseNsightApiSum (json : Json) : Array MemoryEvent := Id.run do
  let mut events : Array MemoryEvent := #[]

  let records := json.getObjValAs? (Array Json) "data" |>.toOption
    |>.getD (json.getArr? |>.toOption |>.getD #[])

  for record in records do
    let name := record.getObjValAs? String "Name" |>.toOption
      |>.orElse (fun _ => record.getObjValAs? String "name" |>.toOption)
      |>.getD ""

    -- Check if this is a memory operation
    let isAlloc := strContains name "Malloc" || strContains name "Alloc"
    let isFree := strContains name "Free"
    let isMemcpy := strContains name "Memcpy" || strContains name "memcpy"

    if isAlloc || isFree || isMemcpy then
      -- Get size info if available
      let totalBytes := record.getObjValAs? Nat "Total (bytes)" |>.toOption
        |>.orElse (fun _ => record.getObjValAs? Nat "bytes" |>.toOption)
        |>.getD 0

      let timeNs := record.getObjValAs? Float "Total Time (ns)" |>.toOption

      let eventType := if isAlloc then "alloc"
        else if isFree then "free"
        else if strContains name "HtoD" then "transfer_h2d"
        else if strContains name "DtoH" then "transfer_d2h"
        else if strContains name "DtoD" then "transfer_d2d"
        else "alloc"

      events := events.push {
        type := eventType
        bytes := totalBytes
        timestampNs := timeNs
        symbol := some name
      }

  return events

/-- Parse NSight gpumemtimesum JSON report. -/
def parseNsightMemTimeSum (json : Json) : Array MemoryEvent := Id.run do
  let mut events : Array MemoryEvent := #[]

  let records := json.getObjValAs? (Array Json) "data" |>.toOption
    |>.getD (json.getArr? |>.toOption |>.getD #[])

  for record in records do
    let operation := record.getObjValAs? String "Operation" |>.toOption
      |>.orElse (fun _ => record.getObjValAs? String "operation" |>.toOption)
      |>.getD ""

    let totalBytes := record.getObjValAs? Nat "Total (bytes)" |>.toOption
      |>.orElse (fun _ => record.getObjValAs? Nat "bytes" |>.toOption)
      |>.getD 0

    let timeNs := record.getObjValAs? Float "Total Time (ns)" |>.toOption

    let eventType := if strContains operation "HtoD" then "transfer_h2d"
      else if strContains operation "DtoH" then "transfer_d2h"
      else if strContains operation "DtoD" then "transfer_d2d"
      else "alloc"

    if totalBytes > 0 then
      events := events.push {
        type := eventType
        bytes := totalBytes
        timestampNs := timeNs
      }

  return events

/-- Convert NSight data to RuntimeData. -/
def nsightToRuntimeData (kernelJson : Option Json) (apiJson : Option Json) : RuntimeData := Id.run do
  let kernels := match kernelJson with
    | some j => parseNsightKernelSum j
    | none => #[]

  let memEvents := match apiJson with
    | some j => parseNsightApiSum j ++ parseNsightMemTimeSum j
    | none => #[]

  return {
    source := { tool := "nsight", platform := some "cuda" }
    gpuInfo := some { vendor := some "nvidia" }
    gpuKernels := kernels
    gpuMemoryEvents := memEvents
  }

/-- Load NSight kernel JSON and convert to MetricByFile. -/
def collectFromNsight (kernelPath : Option System.FilePath) (apiPath : Option System.FilePath := none) : IO MetricByFile := do
  let kernelJson ← match kernelPath with
    | some path =>
      let content ← IO.FS.readFile path
      match Json.parse content with
      | .ok j => pure (some j)
      | .error _ => pure none
    | none => pure none

  let apiJson ← match apiPath with
    | some path =>
      let content ← IO.FS.readFile path
      match Json.parse content with
      | .ok j => pure (some j)
      | .error _ => pure none
    | none => pure none

  let data := nsightToRuntimeData kernelJson apiJson
  let floatMetrics := runtimeDataToMetricsByFile data
  return mergeMetricByFileF {} floatMetrics

end LeanBench.Runtime
