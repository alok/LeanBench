import LeanBench.Runtime.Types
import LeanBench.Runtime.Validator

/-!
# Generic Runtime Collector

Parses vendor-neutral runtime JSON and aggregates metrics by file.
Other collectors (perf, Instruments, Metal) convert to this format first.
-/

namespace LeanBench.Runtime

open Lean (Json)

/-- Aggregate CPU samples into per-file metrics. -/
def aggregateCpuSamples (samples : Array CpuSample) : MetricByFileF := Id.run do
  let mut result : MetricByFileF := {}
  for sample in samples do
    if let some file := sample.file then
      let path := System.FilePath.mk file
      let mut fileMetrics := result.getD path {}

      if let some n := sample.samples then
        let prev := fileMetrics.getD "cpu_samples" 0.0
        fileMetrics := fileMetrics.insert "cpu_samples" (prev + n.toFloat)

      if let some t := sample.timeNs then
        let prev := fileMetrics.getD "cpu_time_ns" 0.0
        fileMetrics := fileMetrics.insert "cpu_time_ns" (prev + t)

      if let some c := sample.cycles then
        let prev := fileMetrics.getD "cpu_cycles" 0.0
        fileMetrics := fileMetrics.insert "cpu_cycles" (prev + c.toFloat)

      if let some i := sample.instructions then
        let prev := fileMetrics.getD "cpu_instructions" 0.0
        fileMetrics := fileMetrics.insert "cpu_instructions" (prev + i.toFloat)

      if let some m := sample.cacheMisses then
        let prev := fileMetrics.getD "cpu_cache_misses" 0.0
        fileMetrics := fileMetrics.insert "cpu_cache_misses" (prev + m.toFloat)

      if let some b := sample.branchMisses then
        let prev := fileMetrics.getD "cpu_branch_misses" 0.0
        fileMetrics := fileMetrics.insert "cpu_branch_misses" (prev + b.toFloat)

      result := result.insert path fileMetrics
  return result

/-- Compute IPC (instructions per cycle) for each file. -/
def computeIpc (metrics : MetricByFileF) : MetricByFileF :=
  metrics.fold (fun acc path m =>
    let cycles := m.getD "cpu_cycles" 0.0
    let instrs := m.getD "cpu_instructions" 0.0
    if cycles > 0 then
      let ipc := instrs / cycles
      let m' := m.insert "cpu_ipc_bp" ipc
      acc.insert path m'
    else
      acc.insert path m
  ) {}

/-- Aggregate GPU kernels into per-file metrics. -/
def aggregateGpuKernels (kernels : Array GpuKernel) : MetricByFileF := Id.run do
  let mut result : MetricByFileF := {}
  for kernel in kernels do
    if let some file := kernel.file then
      let path := System.FilePath.mk file
      let mut fileMetrics := result.getD path {}

      -- Kernel count
      let prevCount := fileMetrics.getD "gpu_kernel_count" 0.0
      fileMetrics := fileMetrics.insert "gpu_kernel_count" (prevCount + 1.0)

      -- Kernel time
      if let some t := kernel.durationNs then
        let prev := fileMetrics.getD "gpu_kernel_time_ns" 0.0
        fileMetrics := fileMetrics.insert "gpu_kernel_time_ns" (prev + t)

      -- Memory read/write
      if let some r := kernel.memoryReadBytes then
        let prev := fileMetrics.getD "gpu_memory_read_bytes" 0.0
        fileMetrics := fileMetrics.insert "gpu_memory_read_bytes" (prev + r.toFloat)

      if let some w := kernel.memoryWriteBytes then
        let prev := fileMetrics.getD "gpu_memory_write_bytes" 0.0
        fileMetrics := fileMetrics.insert "gpu_memory_write_bytes" (prev + w.toFloat)

      -- Occupancy (average)
      if let some occ := kernel.occupancyPercent then
        let prevOcc := fileMetrics.getD "_gpu_occupancy_sum" 0.0
        let prevN := fileMetrics.getD "_gpu_occupancy_n" 0.0
        fileMetrics := fileMetrics.insert "_gpu_occupancy_sum" (prevOcc + occ)
        fileMetrics := fileMetrics.insert "_gpu_occupancy_n" (prevN + 1.0)

      -- Bandwidth (average)
      if let some bw := kernel.achievedBandwidthGbps then
        let prevBw := fileMetrics.getD "_gpu_bw_sum" 0.0
        let prevN := fileMetrics.getD "_gpu_bw_n" 0.0
        fileMetrics := fileMetrics.insert "_gpu_bw_sum" (prevBw + bw)
        fileMetrics := fileMetrics.insert "_gpu_bw_n" (prevN + 1.0)

      result := result.insert path fileMetrics
  return result

/-- Finalize GPU metrics: compute averages for occupancy/bandwidth. -/
def finalizeGpuMetrics (metrics : MetricByFileF) : MetricByFileF :=
  metrics.fold (fun acc path m => Id.run do
    let mut m' := m

    -- Average occupancy -> basis points
    let occSum := m.getD "_gpu_occupancy_sum" 0.0
    let occN := m.getD "_gpu_occupancy_n" 0.0
    if occN > 0 then
      let avgOcc := occSum / occN / 100.0  -- Convert percent to ratio
      m' := m'.insert "gpu_occupancy_bp" avgOcc
    m' := m'.erase "_gpu_occupancy_sum"
    m' := m'.erase "_gpu_occupancy_n"

    -- Average bandwidth -> basis points (relative to 1000 GB/s baseline)
    let bwSum := m.getD "_gpu_bw_sum" 0.0
    let bwN := m.getD "_gpu_bw_n" 0.0
    if bwN > 0 then
      let avgBw := bwSum / bwN / 1000.0  -- Ratio relative to 1000 GB/s
      m' := m'.insert "gpu_bandwidth_gbps_bp" avgBw
    m' := m'.erase "_gpu_bw_sum"
    m' := m'.erase "_gpu_bw_n"

    return acc.insert path m'
  ) {}

/-- Aggregate memory events into per-file metrics. -/
def aggregateMemoryEvents (events : Array MemoryEvent) : MetricByFileF := Id.run do
  let mut result : MetricByFileF := {}
  for event in events do
    if let some file := event.file then
      let path := System.FilePath.mk file
      let mut fileMetrics := result.getD path {}

      match event.type with
      | "alloc" =>
        let prevCount := fileMetrics.getD "alloc_count" 0.0
        let prevBytes := fileMetrics.getD "alloc_bytes" 0.0
        fileMetrics := fileMetrics.insert "alloc_count" (prevCount + 1.0)
        fileMetrics := fileMetrics.insert "alloc_bytes" (prevBytes + event.bytes.toFloat)
      | "free" =>
        let prev := fileMetrics.getD "free_count" 0.0
        fileMetrics := fileMetrics.insert "free_count" (prev + 1.0)
      | _ => pure ()

      result := result.insert path fileMetrics
  return result

/-- Aggregate FFI calls into per-file metrics. -/
def aggregateFfiCalls (calls : Array FfiCall) : MetricByFileF := Id.run do
  let mut result : MetricByFileF := {}
  for call in calls do
    -- Use lean_decl to map to file if available
    -- For now, aggregate globally under a synthetic path
    let path := System.FilePath.mk "_ffi"
    let mut fileMetrics := result.getD path {}

    if let some n := call.callCount then
      let prev := fileMetrics.getD "ffi_call_count" 0.0
      fileMetrics := fileMetrics.insert "ffi_call_count" (prev + n.toFloat)

    if let some t := call.totalTimeNs then
      let prev := fileMetrics.getD "ffi_time_ns" 0.0
      fileMetrics := fileMetrics.insert "ffi_time_ns" (prev + t)

    result := result.insert path fileMetrics
  return result

/-- Merge multiple MetricByFileF maps. -/
def mergeMetricsByFile (maps : Array MetricByFileF) : MetricByFileF :=
  maps.foldl (fun acc m =>
    m.fold (fun acc' path metrics =>
      let existing := acc'.getD path {}
      let merged := metrics.fold (fun e k v =>
        let prev := e.getD k 0.0
        e.insert k (prev + v)
      ) existing
      acc'.insert path merged
    ) acc
  ) {}

/-- Convert RuntimeData to MetricByFileF (float metrics by file). -/
def runtimeDataToMetricsByFile (data : RuntimeData) : MetricByFileF :=
  let cpuMetrics := computeIpc (aggregateCpuSamples data.cpuSamples)
  let gpuMetrics := finalizeGpuMetrics (aggregateGpuKernels data.gpuKernels)
  let memMetrics := aggregateMemoryEvents data.memoryEvents
  let gpuMemMetrics := aggregateMemoryEvents data.gpuMemoryEvents
  let ffiMetrics := aggregateFfiCalls data.ffiCalls
  mergeMetricsByFile #[cpuMetrics, gpuMetrics, memMetrics, gpuMemMetrics, ffiMetrics]

/-- Load runtime JSON, validate, and convert to MetricByFile (Nat metrics). -/
def collectFromRuntimeJson (path : System.FilePath) : IO MetricByFile := do
  let rawData ← readRuntimeJson path
  let data ← validateAndLogWarnings rawData
  let floatMetrics := runtimeDataToMetricsByFile data
  return mergeMetricByFileF {} floatMetrics

end LeanBench.Runtime
