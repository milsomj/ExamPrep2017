t :: [a] -> [a]
t (x:xs) = xs

(+++) :: [a] -> [a] -> [a]
(+++) [] ys = ys
(+++) (x:xs) ys =  x: xs +++ ys

minit :: [a] -> [a]
minit (x:[]) = []
minit (x:xs) = x : minit xs

mbreak :: (a -> Bool) -> [a] -> ([a],[a])
mbreak bool (x:xs)
  | bool x = ((x:y),ys)
  | otherwise = ([], (x:xs))
   where (y,ys) = mbreak bool xs

mreverse :: [a] -> [a]
mreverse [] = []
mreverse (x:xs) = mreverse xs ++ [x]

mmax :: Ord a => [a] -> a
mmax (x:[]) = x
mmax (x:xs)
  | x > b = x
  | otherwise = b
  where b = mmax xs
