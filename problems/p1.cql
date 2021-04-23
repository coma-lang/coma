-- PROBLEM 1


let A := read "test/data/p1-2-A.csv" in
let B := read "test/data/p1-2-B.csv" in
csv (join A B)
