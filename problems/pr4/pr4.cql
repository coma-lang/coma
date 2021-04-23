-- PROBLEM 4


let A := read "A.csv" in
csv ( given (\row -> value 1 row != "") A )
