-- PROBLEM 5


let A := read "A.csv" in
csv ( each (\row -> row ++ ["0"] ++ row) A )
