let S := read "S.csv" in
let T := read "T.csv" in
let J := join S T in

-- row : s1 s2 s3  t1 t2 t3
-- ids : 0  1  2   3  4  5
let predicate := \row ->
  let s1 := value 0 row in
  let s2 := value 1 row in
  let t2 := value 4 row in
  let t3 := value 5 row in
  s1 = s2 & t2 = t3
in

csv (select [2 3] (given predicate J))
