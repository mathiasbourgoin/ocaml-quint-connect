// Counter spec: simple counter with increment action and nonnegativity invariant
module counter {
  var counter: int

  action init = { counter' = 0 }

  action increment = { counter' = counter + 1 }

  invariant counterNonNegative = counter >= 0
}
