-- PROBLEM 5


let A := read "A.csv" in
csv ( forEach (\row -> row ++ ["0"] ++ row) A )
