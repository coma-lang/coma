-- PROBLEM 1


let A := read "A.csv" in
let B := read "B.csv" in
csv (join A B)
