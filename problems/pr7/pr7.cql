let F := read "F.csv" in
let G := read "G.csv" in
let J := join F G in

-- row : f1 f2 f3  g1 g2 g3
-- ids : 0  1  2   3  4  5
let predicate := \row ->
  let f2 := value 1 row in
  let f3 := value 2 row in
  let g1 := value 3 row in
  let g2 := value 4 row in
  f2 != "" & f3 != "" & f2 = g1 & f3 = g2
in

csv (select [0 5] (given predicate J))