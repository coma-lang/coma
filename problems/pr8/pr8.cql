let P := read "P.csv" in
let Q := read "Q.csv" in

let filtered := given (\row -> value 3 row = value 7 row) (join P Q) in

-- row : p1 p2 p3 p4  q1 q2 q3 q4
-- ids : 0  1  2  3   4  5  6  7
let merged := \row ->
  let ps := get [0 1 2 3] row in
  let qs := get [4 5 6 7] row in
  merge qs ps
in

csv (each merged filtered)
