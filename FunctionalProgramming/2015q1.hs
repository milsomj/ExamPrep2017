-- repeat ie. infinite list
r :: a -> [a]
r a = (a: r a)

-- replicate a for n iterations
re :: Int -> a -> [a]
re 0 a = []
re n a = (a:re (n-1) a)

-- concat, join lists of lists into one big list
mconcat :: [[a]] -> [a]
mconcat [] = []
mconcat [[a]] = [a]
mconcat ([a]:xs) = [a] ++ mconcat xs

-- zip
z :: [a] -> [b] -> [(a,b)]
z [] [] = []
z (x:xs) (y:ys) = ((x,y):z xs ys)

-- unzip
uz :: [(a,b)] -> ([a],[b])
uz [] = ([],[])
uz ((h,hs):xs) = ((h:t),(hs:ts)) where (t,ts) = uz (xs)

-- find minimum in list
m :: (Ord a) => [a] -> a
m [a] = a
m (x:xs)
  | x > b = b
  | otherwise = x
  where b = m xs
