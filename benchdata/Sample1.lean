import Std

open Std

def fib : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => fib (n + 1) + fib n

def sumList (xs : List Nat) : Nat :=
  xs.foldl (fun acc x => acc + x) 0

example : fib 5 = 5 := by
  decide

example (xs : List Nat) : sumList xs = xs.foldl (· + ·) 0 := by
  rfl
