--1.
--(a)
head :: [a] -> a
head (x:_) = x

--(b)
init :: [a] -> [a]
init (x:[]) = []
init (x:xs) = x:(init xs)

--(c)
last :: [a] -> a
last (x:[]) = x
last (x:xs) = last(xs)

--(d)
span :: (a -> Bool) ->[a] -> ([a],[a])
span bool [] = ([],[])
span bool (x:xs)
   | bool x = let(d,e) = span bool xs in (x:d,e)
   | otherwise = ([],(x:xs))

--(e)
(!!) :: [a] -> Int -> a
(!!) (x:xs) a
    | a == 0 = x
    | otherwise = xs!!(a-1)

--(f)
f :: (a -> a -> a) -> [a] -> a
f func [] = error "Prelude.foldl1: empty list"
f func (x:[]) = x
f func (x:xs:[]) = func x xs
f func (x:xs:ys) = f func ((func x xs):ys)
