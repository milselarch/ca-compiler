structure BiDirectionalTape where
  /- cells extending rightwards (positions increasing) -/
  data : Array Nat
  /- cells extending leftwards (positions decreasing) -/
  rev_data : Array Nat

/-- Adds a 0 to the end of the rightwards data array -/
def rightBorderSucc (t : BiDirectionalTape) : BiDirectionalTape :=
  { t with data := t.data.push 0 }

/-- Adds a 0 to the end of the leftwards rev_data array -/
def leftBorderSucc (t : BiDirectionalTape) : BiDirectionalTape :=
  { t with rev_data := t.rev_data.push 0 }

/-- Removes the last item from the rightwards data array, if it exists -/
def removeRight (t : BiDirectionalTape) : BiDirectionalTape :=
  match t.data.pop? with
  | some (_, newData) => { t with data := newData }
  | none => t

structure Term where
  position : Int
  state : Nat

structure Product where
  terms: Array Term

structure Expression where
  products: Array Product
