-- PROBLEM 2


let A := read "A.csv" in
let eq := given (\row -> value 0 row = value 1 row) A in
csv (select [2 0] eq)
