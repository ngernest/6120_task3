open Syntax

let full (fn: func) : func =
  let fn' = Lvn.lvn fn in
  Tdce.tdce_plus fn'
