getAtIndex :: Int -> [a] -> a
getAtIndex i [] = error "Out of bounds"
getAtIndex 0 (x:xs) = x
getAtIndex i (x:xs) = getAtIndex (i-1) xs
