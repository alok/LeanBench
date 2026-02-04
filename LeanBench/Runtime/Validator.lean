import LeanBench.Runtime.Types

/-!
# Runtime Metrics Validation

Validates runtime profiling data for consistency and reasonable ranges.
Logs warnings but doesn't fail - returns cleaned data.
-/

namespace LeanBench.Runtime

/-- Validation warning with context. -/
structure ValidationWarning where
  category : String  -- "range" | "consistency" | "suspicious"
  field : String
  message : String
  value : Option Float := none
  deriving Inhabited, Repr

/-- Validation result with warnings. -/
structure ValidationResult (α : Type) where
  data : α
  warnings : Array ValidationWarning := #[]
  deriving Inhabited

instance [Inhabited α] : Inhabited (ValidationResult α) where
  default := { data := default, warnings := #[] }

/-- Check if a float is in a valid range. -/
def inRange (val : Float) (min max : Float) : Bool :=
  val >= min && val <= max

/-- Validate a GPU kernel. Returns the kernel (possibly clamped) and warnings. -/
def validateGpuKernel (kernel : GpuKernel) : ValidationResult GpuKernel := Id.run do
  let mut warnings : Array ValidationWarning := #[]
  let mut k := kernel

  -- Occupancy should be 0-100%
  if let some occ := kernel.occupancyPercent then
    if occ < 0.0 then
      warnings := warnings.push {
        category := "range"
        field := "occupancy_percent"
        message := s!"Occupancy {occ}% is negative, clamping to 0"
        value := some occ
      }
      k := { k with occupancyPercent := some 0.0 }
    else if occ > 100.0 then
      warnings := warnings.push {
        category := "range"
        field := "occupancy_percent"
        message := s!"Occupancy {occ}% exceeds 100%, clamping"
        value := some occ
      }
      k := { k with occupancyPercent := some 100.0 }

  -- Duration should be non-negative
  if let some dur := kernel.durationNs then
    if dur < 0.0 then
      warnings := warnings.push {
        category := "range"
        field := "duration_ns"
        message := s!"Duration {dur}ns is negative, setting to 0"
        value := some dur
      }
      k := { k with durationNs := some 0.0 }
    -- Warn if duration is suspiciously long (> 10 seconds)
    else if dur > 10000000000.0 then
      warnings := warnings.push {
        category := "suspicious"
        field := "duration_ns"
        message := s!"Duration {dur / 1000000000.0}s seems unusually long"
        value := some dur
      }

  -- Start time should be non-negative
  if let some start := kernel.startNs then
    if start < 0.0 then
      warnings := warnings.push {
        category := "range"
        field := "start_ns"
        message := s!"Start time {start}ns is negative, setting to 0"
        value := some start
      }
      k := { k with startNs := some 0.0 }

  -- Bandwidth should be non-negative
  if let some bw := kernel.achievedBandwidthGbps then
    if bw < 0.0 then
      warnings := warnings.push {
        category := "range"
        field := "achieved_bandwidth_gbps"
        message := s!"Bandwidth {bw} GB/s is negative, setting to 0"
        value := some bw
      }
      k := { k with achievedBandwidthGbps := some 0.0 }

  return { data := k, warnings := warnings }

/-- Validate a CPU sample. -/
def validateCpuSample (sample : CpuSample) : ValidationResult CpuSample := Id.run do
  let mut warnings : Array ValidationWarning := #[]
  let mut s := sample

  -- Time should be non-negative
  if let some time := sample.timeNs then
    if time < 0.0 then
      warnings := warnings.push {
        category := "range"
        field := "time_ns"
        message := s!"CPU time {time}ns is negative, setting to 0"
        value := some time
      }
      s := { s with timeNs := some 0.0 }

  -- Samples should be positive (can't validate Nat negativity, but check for 0)
  if let some samples := sample.samples then
    if samples == 0 then
      warnings := warnings.push {
        category := "suspicious"
        field := "samples"
        message := "CPU sample count is 0"
        value := some 0.0
      }

  return { data := s, warnings := warnings }

/-- Validate a memory event. -/
def validateMemoryEvent (event : MemoryEvent) : ValidationResult MemoryEvent := Id.run do
  let mut warnings : Array ValidationWarning := #[]
  let e := event

  -- Type should be valid
  let validTypes := #["alloc", "free", "realloc", "gc", "transfer_h2d", "transfer_d2h", "transfer_d2d"]
  if !validTypes.contains event.type then
    warnings := warnings.push {
      category := "range"
      field := "type"
      message := s!"Unknown memory event type: {event.type}"
    }

  -- Bytes = 0 is suspicious for alloc
  if event.bytes == 0 && event.type == "alloc" then
    warnings := warnings.push {
      category := "suspicious"
      field := "bytes"
      message := "Zero-byte allocation"
      value := some 0.0
    }

  return { data := e, warnings := warnings }

/-- Check consistency between GPU kernels. -/
def checkKernelConsistency (kernels : Array GpuKernel) : Array ValidationWarning := Id.run do
  let mut warnings : Array ValidationWarning := #[]

  -- Check for overlapping kernels on same stream (if timing info present)
  let kernelsWithTiming := kernels.filter fun k =>
    k.startNs.isSome && k.durationNs.isSome

  for i in [:kernelsWithTiming.size] do
    for j in [i+1:kernelsWithTiming.size] do
      let k1 := kernelsWithTiming[i]!
      let k2 := kernelsWithTiming[j]!

      -- Only check same stream
      if k1.stream == k2.stream then
        let start1 := k1.startNs.getD 0.0
        let end1 := start1 + k1.durationNs.getD 0.0
        let start2 := k2.startNs.getD 0.0
        let end2 := start2 + k2.durationNs.getD 0.0

        -- Check overlap
        if start1 < end2 && start2 < end1 then
          warnings := warnings.push {
            category := "consistency"
            field := "timing"
            message := s!"Kernels '{k1.name}' and '{k2.name}' overlap on stream {k1.stream.getD 0}"
          }

  return warnings

/-- Validate entire RuntimeData. -/
def validateRuntimeData (data : RuntimeData) : ValidationResult RuntimeData := Id.run do
  let mut warnings : Array ValidationWarning := #[]
  let mut validatedData := data

  -- Validate GPU kernels
  let mut validatedKernels : Array GpuKernel := #[]
  for kernel in data.gpuKernels do
    let result := validateGpuKernel kernel
    validatedKernels := validatedKernels.push result.data
    warnings := warnings ++ result.warnings
  validatedData := { validatedData with gpuKernels := validatedKernels }

  -- Check kernel consistency
  warnings := warnings ++ checkKernelConsistency validatedKernels

  -- Validate CPU samples
  let mut validatedSamples : Array CpuSample := #[]
  for sample in data.cpuSamples do
    let result := validateCpuSample sample
    validatedSamples := validatedSamples.push result.data
    warnings := warnings ++ result.warnings
  validatedData := { validatedData with cpuSamples := validatedSamples }

  -- Validate memory events
  let mut validatedEvents : Array MemoryEvent := #[]
  for event in data.memoryEvents do
    let result := validateMemoryEvent event
    validatedEvents := validatedEvents.push result.data
    warnings := warnings ++ result.warnings
  validatedData := { validatedData with memoryEvents := validatedEvents }

  -- Also validate GPU memory events
  let mut validatedGpuEvents : Array MemoryEvent := #[]
  for event in data.gpuMemoryEvents do
    let result := validateMemoryEvent event
    validatedGpuEvents := validatedGpuEvents.push result.data
    warnings := warnings ++ result.warnings
  validatedData := { validatedData with gpuMemoryEvents := validatedGpuEvents }

  -- Cross-check: total kernel time vs frame GPU time (if frames present)
  if data.frames.size > 0 then
    let totalKernelTime := data.gpuKernels.foldl (fun acc k => acc + k.durationNs.getD 0.0) 0.0
    let totalFrameGpuTime := data.frames.foldl (fun acc f => acc + f.gpuTimeNs.getD 0.0) 0.0

    if totalFrameGpuTime > 0.0 && totalKernelTime > totalFrameGpuTime * 1.1 then
      warnings := warnings.push {
        category := "consistency"
        field := "gpu_time"
        message := s!"Total kernel time ({totalKernelTime}ns) exceeds total frame GPU time ({totalFrameGpuTime}ns) by >10%"
      }

  return { data := validatedData, warnings := warnings }

/-- Validate RuntimeData and log warnings to IO. -/
def validateAndLogWarnings (data : RuntimeData) : IO RuntimeData := do
  let result := validateRuntimeData data
  for warning in result.warnings do
    IO.eprintln s!"[{warning.category}] {warning.field}: {warning.message}"
  return result.data

end LeanBench.Runtime
