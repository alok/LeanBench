import Std

open Std

structure Point where
  x : Int
  y : Int
  deriving Repr, DecidableEq

def dist2 (p q : Point) : Int :=
  let dx := p.x - q.x
  let dy := p.y - q.y
  dx*dx + dy*dy

example (p : Point) : dist2 p p = 0 := by
  simp [dist2]

example (p q : Point) : dist2 p q = dist2 p q := by
  simp [dist2]
