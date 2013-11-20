--Column Storage
type Vector a = [a]
type Matrix a = [[a]]

dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct x y
	| length x /= length y = error "Dimensions mismatch"
	| otherwise = foldl (+) 0 (cross (*) x y)

cross f [] [] = []
cross f (x:xs) (y:ys) = (f x y):(cross f xs ys)

crossProduct x y = cross (*) x y

columnToRow :: Matrix a -> Matrix a
columnToRow x = map (nColumn x) [0..length x-1]

nColumn a n = map (\x -> x !! n) a

product :: Num a => Matrix a -> Matrix a -> Matrix a
product x y
	| length x /= length (columnToRow y) = error "Column/Row mismatch"
	| otherwise = map (\a ->  subProduct a y) x

subProduct x y = map (dotProduct x) (columnToRow y)


