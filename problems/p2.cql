-- PROBLEM 2


let A := read "test/data/p2-3-A.csv" in
select [2 0] (given (\row -> value 0 row = value 1 row) A)
