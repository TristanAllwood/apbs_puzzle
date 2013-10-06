module ABPS where

{- there are two numbers each between 2 and 99 inclusive. -}
{- call them a and b -}

{- there are two participants, p and s. -}
{- p only knows the product of a and b -}
{- s only knows the sum of a and b -}
p0 (a, b) = a * b
s0 (a, b) = a + b

{- p says "I don't know a and b" -}
p1 = multiple_solutions . factors . p0

{- s says "I know you don't know" -}
s1 = all p1 . components . s0

{- p now says "I know what the numbers are" -}
p2 = single_solution . filter s1 . factors . p0

{- s finally says "I know what the numbers are" -}
s2 = single_solution . filter p2 . components . s0


solution = [ ab | a <- [2..99], b <- [2..a],
                  let ab = (a,b),
                  p1 ab,
                  s1 ab,
                  p2 ab,
                  s2 ab
           ]

single_solution :: [a] -> Bool
single_solution [_] = True
single_solution _   = False

multiple_solutions :: [a] -> Bool
multiple_solutions = not . null . drop 1

factors :: Int -> [(Int, Int)]
factors n = [ (x,y) | x <- [2..min 99 (floor . sqrt . fromIntegral $ n)],
                      (n `mod` x) == 0,
                      let y = n `div` x,
                      y >= 2 && y <= 99
            ]

components :: Int -> [(Int, Int)]
components n = [ (x,y) | x <- [2..min 99 ((n `div` 2) + 1)],
                         let y = n - x,
                         y >= x && y >= 2 && y <= 99
               ]
