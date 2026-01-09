import Lean.Data.Json
import LeanBench.TextScan

/-!
# Runtime Profiling Types

Shared types for CPU/GPU runtime profiling collectors.
These types mirror the vendor-neutral JSON schema for import.
-/

namespace LeanBench.Runtime

open Lean (Json)

/-- Profiler source information. -/
structure SourceInfo where
  tool : String
  version : Option String := none
  platform : Option String := none
  captureTime : Option String := none
  deriving Inhabited

/-- CPU device information. -/
structure CpuInfo where
  name : Option String := none
  cores : Option Nat := none
  threads : Option Nat := none
  frequencyMhz : Option Float := none
  deriving Inhabited

/-- GPU device information. -/
structure GpuInfo where
  name : Option String := none
  vendor : Option String := none
  computeUnits : Option Nat := none
  memoryBytes : Option Nat := none
  peakBandwidthGbps : Option Float := none
  deriving Inhabited

/-- CPU profiling sample (one stack frame or function). -/
structure CpuSample where
  symbol : String
  leanDecl : Option String := none
  file : Option String := none
  line : Option Nat := none
  samples : Option Nat := none
  timeNs : Option Float := none
  cycles : Option Nat := none
  instructions : Option Nat := none
  cacheMisses : Option Nat := none
  branchMisses : Option Nat := none
  deriving Inhabited

/-- GPU kernel execution record. -/
structure GpuKernel where
  name : String
  leanDecl : Option String := none
  file : Option String := none
  line : Option Nat := none
  stream : Option Nat := none
  startNs : Option Float := none
  durationNs : Option Float := none
  gridSize : Option (Array Nat) := none
  blockSize : Option (Array Nat) := none
  registersPerThread : Option Nat := none
  sharedMemoryBytes : Option Nat := none
  occupancyPercent : Option Float := none
  memoryReadBytes : Option Nat := none
  memoryWriteBytes : Option Nat := none
  achievedBandwidthGbps : Option Float := none
  flops : Option Nat := none
  deriving Inhabited

/-- Memory event (CPU or GPU allocation). -/
structure MemoryEvent where
  type : String  -- "alloc" | "free" | "realloc" | "gc" | "transfer_h2d" | "transfer_d2h" | "transfer_d2d"
  bytes : Nat
  timestampNs : Option Float := none
  symbol : Option String := none
  leanDecl : Option String := none
  file : Option String := none
  line : Option Nat := none
  address : Option String := none
  deriving Inhabited

/-- FFI call record. -/
structure FfiCall where
  externName : String
  leanDecl : Option String := none
  callCount : Option Nat := none
  totalTimeNs : Option Float := none
  minTimeNs : Option Float := none
  maxTimeNs : Option Float := none
  avgTimeNs : Option Float := none
  gpuKernel : Option String := none
  deriving Inhabited

/-- Frame-by-frame data for time-series. -/
structure FrameData where
  frameId : Nat
  startNs : Option Float := none
  durationNs : Option Float := none
  cpuTimeNs : Option Float := none
  gpuTimeNs : Option Float := none
  cpuSamples : Option Nat := none
  gpuKernelCount : Option Nat := none
  memoryAllocBytes : Option Nat := none
  memoryFreeBytes : Option Nat := none
  deriving Inhabited

/-- Complete runtime metrics data from a profiler. -/
structure RuntimeData where
  schemaVersion : String := "0.1.0"
  source : SourceInfo
  cpuInfo : Option CpuInfo := none
  gpuInfo : Option GpuInfo := none
  cpuSamples : Array CpuSample := #[]
  gpuKernels : Array GpuKernel := #[]
  memoryEvents : Array MemoryEvent := #[]
  gpuMemoryEvents : Array MemoryEvent := #[]
  ffiCalls : Array FfiCall := #[]
  frames : Array FrameData := #[]
  aggregatedByFile : Std.HashMap String MetricMapF := {}
  aggregatedByDecl : Std.HashMap String MetricMapF := {}
  deriving Inhabited

/-- Parse JSON to RuntimeData. -/
def parseRuntimeJson (json : Json) : Except String RuntimeData := do
  let schemaVersion := json.getObjValAs? String "schema_version" |>.toOption |>.getD "0.1.0"

  -- Parse source
  let sourceObj := json.getObjValAs? Json "source" |>.toOption |>.getD (Json.mkObj [])
  let tool := sourceObj.getObjValAs? String "tool" |>.toOption |>.getD "custom"
  let source : SourceInfo := {
    tool := tool
    version := sourceObj.getObjValAs? String "version" |>.toOption
    platform := sourceObj.getObjValAs? String "platform" |>.toOption
    captureTime := sourceObj.getObjValAs? String "capture_time" |>.toOption
  }

  -- Parse CPU samples
  let cpuSamplesJson := json.getObjValAs? (Array Json) "cpu_samples" |>.toOption |>.getD #[]
  let cpuSamples := cpuSamplesJson.filterMap fun j =>
    match j.getObjValAs? String "symbol" with
    | .ok sym => some {
        symbol := sym
        leanDecl := j.getObjValAs? String "lean_decl" |>.toOption
        file := j.getObjValAs? String "file" |>.toOption
        line := j.getObjValAs? Nat "line" |>.toOption
        samples := j.getObjValAs? Nat "samples" |>.toOption
        timeNs := j.getObjValAs? Float "time_ns" |>.toOption
        cycles := j.getObjValAs? Nat "cycles" |>.toOption
        instructions := j.getObjValAs? Nat "instructions" |>.toOption
        cacheMisses := j.getObjValAs? Nat "cache_misses" |>.toOption
        branchMisses := j.getObjValAs? Nat "branch_misses" |>.toOption
      }
    | .error _ => none

  -- Parse GPU kernels
  let gpuKernelsJson := json.getObjValAs? (Array Json) "gpu_kernels" |>.toOption |>.getD #[]
  let gpuKernels := gpuKernelsJson.filterMap fun j =>
    match j.getObjValAs? String "name" with
    | .ok name => some {
        name := name
        leanDecl := j.getObjValAs? String "lean_decl" |>.toOption
        file := j.getObjValAs? String "file" |>.toOption
        line := j.getObjValAs? Nat "line" |>.toOption
        stream := j.getObjValAs? Nat "stream" |>.toOption
        startNs := j.getObjValAs? Float "start_ns" |>.toOption
        durationNs := j.getObjValAs? Float "duration_ns" |>.toOption
        occupancyPercent := j.getObjValAs? Float "occupancy_percent" |>.toOption
        memoryReadBytes := j.getObjValAs? Nat "memory_read_bytes" |>.toOption
        memoryWriteBytes := j.getObjValAs? Nat "memory_write_bytes" |>.toOption
        achievedBandwidthGbps := j.getObjValAs? Float "achieved_bandwidth_gbps" |>.toOption
        flops := j.getObjValAs? Nat "flops" |>.toOption
      }
    | .error _ => none

  -- Parse memory events
  let memEventsJson := json.getObjValAs? (Array Json) "memory_events" |>.toOption |>.getD #[]
  let memoryEvents := memEventsJson.filterMap fun j =>
    match j.getObjValAs? String "type", j.getObjValAs? Nat "bytes" with
    | .ok t, .ok b => some {
        type := t
        bytes := b
        timestampNs := j.getObjValAs? Float "timestamp_ns" |>.toOption
        symbol := j.getObjValAs? String "symbol" |>.toOption
        leanDecl := j.getObjValAs? String "lean_decl" |>.toOption
        file := j.getObjValAs? String "file" |>.toOption
        line := j.getObjValAs? Nat "line" |>.toOption
      }
    | _, _ => none

  -- Parse FFI calls
  let ffiJson := json.getObjValAs? (Array Json) "ffi_calls" |>.toOption |>.getD #[]
  let ffiCalls := ffiJson.filterMap fun j =>
    match j.getObjValAs? String "extern_name" with
    | .ok name => some {
        externName := name
        leanDecl := j.getObjValAs? String "lean_decl" |>.toOption
        callCount := j.getObjValAs? Nat "call_count" |>.toOption
        totalTimeNs := j.getObjValAs? Float "total_time_ns" |>.toOption
        minTimeNs := j.getObjValAs? Float "min_time_ns" |>.toOption
        maxTimeNs := j.getObjValAs? Float "max_time_ns" |>.toOption
        avgTimeNs := j.getObjValAs? Float "avg_time_ns" |>.toOption
        gpuKernel := j.getObjValAs? String "gpu_kernel" |>.toOption
      }
    | .error _ => none

  return {
    schemaVersion := schemaVersion
    source := source
    cpuSamples := cpuSamples
    gpuKernels := gpuKernels
    memoryEvents := memoryEvents
    ffiCalls := ffiCalls
  }

/-- Read and parse a runtime metrics JSON file. -/
def readRuntimeJson (path : System.FilePath) : IO RuntimeData := do
  let content ← IO.FS.readFile path
  match Json.parse content with
  | .ok json =>
    match parseRuntimeJson json with
    | .ok data => return data
    | .error e => throw (IO.userError s!"Failed to parse runtime JSON: {e}")
  | .error e => throw (IO.userError s!"Invalid JSON: {e}")

end LeanBench.Runtime
