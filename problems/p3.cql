-- PROBLEM 3


let P := read "test/data/p3-1-P.csv" in
let Q := read "test/data/p3-1-Q.csv" in


let filtered := given (\row -> value 0 row = value 4 row) (join P Q) in

-- row : p1 p2 p3 p4  q1 q2 q3 q4
-- ids : 0  1  2  3   4  5  6  7
let table := 
  forEach ( \row -> merge (get [0 1 2 3] row) (get [4 5 6 7] row) ) filtered
in

csv table
