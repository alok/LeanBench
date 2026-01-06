import LeanBench.TextScan

namespace LeanBench.ParsecScan

@[inline] def tokenizeLineParsec (line : String) : List String :=
  _root_.tokenizeLineParsec line

@[inline] def scanLinesParsec (lines : List String) : ScanAcc :=
  _root_.scanLinesParsec lines

end LeanBench.ParsecScan
