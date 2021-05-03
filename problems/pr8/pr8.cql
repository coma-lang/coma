-- PROBLEM 8


let P := read "P.csv" in
let Q := read "Q.csv" in

-- row : p1 p2 p3 p4  q1 q2 q3 q4
-- ids : 0  1  2  3   4  5  6  7

--What I've changed
let filtered := given (\row -> value 3 row = value 7 row) (join P Q) in

let table := 
  each ( \row -> merge (get [0 1 2 3] row) (get [4 5 6 7] row) ) filtered
in

csv table



--let predicate := \row ->
--  let p4 := value 3 row in
--  let q4 := value 7 row in
--  p4 = q4
--in

