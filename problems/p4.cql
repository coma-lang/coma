-- PROBLEM 4


let A := read "test/data/p4-2-A.csv" in
csv ( given (\row -> value 1 row != "") A )
