-- PROBLEM 4


let A := read "A.csv" in
given (row -> not (empty (value 1 row))) A
