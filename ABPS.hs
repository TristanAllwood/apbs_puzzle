module ABPS where

{- there are two numbers each between 2 and 99 inclusive. -}
{- call them a and b -}

{- there are two participants, p and s. -}
{- p only knows the product of a and b -}
{- s only knows the sum of a and b -}


p (a, b) = multiplicands (a * b)
s (a, b) = summands (a + b)
{- p and s give the possible (a,b)'s for p and s. -}

{- p says "I don't know a and b" -}
p1 = more_than_one . p

{- s says "I know you don't know" -}
s1 = all p1 . s

{- p now says "I know what the numbers are" -}
p2 = exactly_one . filter s1 . p

{- s finally says "I know what the numbers are" -}
s2 = exactly_one . filter p2 . s


solution = [ (a,b)
           | a <- [2..99], b <- [2..a]
           , p1 (a,b)
           , s1 (a,b)
           , p2 (a,b)
           , s2 (a,b)
           ]

exactly_one :: [a] -> Bool
exactly_one [_] = True
exactly_one _   = False

more_than_one :: [a] -> Bool
more_than_one = not . null . drop 1

multiplicands :: Int -> [(Int, Int)]
multiplicands n = [ (x,y) | x <- [2..min 99 (floor . sqrt . fromIntegral $ n)],
                      (n `mod` x) == 0,
                      let y = n `div` x,
                      y >= 2 && y <= 99
            ]

summands :: Int -> [(Int, Int)]
summands n = [ (x,y) | x <- [2..min 99 (n `div` 2)],
                         let y = n - x,
                         y >= x && y >= 2 && y <= 99
               ]
