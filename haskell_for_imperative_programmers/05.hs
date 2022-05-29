-- elem: true iff element in list; must take element and list
elem :: (Eq a) a -> [a] -> Bool
elem _ [] = False
elem e x:xs = match || (elem e xs)
	where match = e == x
-- or
elem _ [] = False
elem e x:xs
 | e == x = True
 | otherwise = elem e xs

-- nub: remove duplicates from a list in any order
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub x:xs
 | (elem x xs) 	= nub xs
 | otherwise 		= x : nub xs

-- is-asc: True iff list is ascending
is-asc :: [Int] -> Bool
is-asc [] = False
is-asc [x] = True
is-asc (x:xs)
 | x > (fst xs) = False
 | otherwise = True && $ is-asc xs
-- or
is-asc (x:y:xs) =
	(x <= y) && $ is-asc (y:xs)

