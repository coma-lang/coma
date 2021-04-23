-- PROBLEM 5


let A := read "test/data/p5-1-A.csv" in
csv ( forEach (\row -> row ++ ["0"] ++ row) A )
