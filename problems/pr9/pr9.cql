let R := read "R.csv" in
let J := join R (join R R) in

-- row : p1 p2 p3 p4 p5 p6
-- ids : 0  1  2  3  4  5
let predicate := \row ->
  let p2 := value 1 row in
  let p3 := value 2 row in
  let p4 := value 3 row in
  let p5 := value 4 row in
  p2 = p3 & p4 = p5
in

csv (select [0 5] (given predicate J))
