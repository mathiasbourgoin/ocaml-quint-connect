// Fixture for Quint 0.32 `run --mbt` traces: nondeterministic picks and an instance
// module, so that state variables are qualified (inst::counter::x).
module counter {
  const STEP: int
  var x: int
  action init = x' = 0
  action add(n: int): bool = x' = x + n * STEP
  action reset: bool = x' = 0
  action step = any {
    nondet n = oneOf(Set(1, 2))
    add(n),
    reset,
  }
}

module inst {
  import counter(STEP = 1).*
}
