let P := read "P.csv" in
let Q := read "Q.csv" in
let R := read "R.csv" in
let S := read "S.csv" in
let T := read "T.csv" in
csv (join (join (join (join P Q) R) S) T)