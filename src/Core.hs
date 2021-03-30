module Core (cartesianProduct) where



-- CARTESIAN PRODUCT


cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]
