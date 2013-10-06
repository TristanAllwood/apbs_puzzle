module ABPS where

import Control.Monad

solution = do
  {- there are two numbers each between 2 and 99 inclusive. -}
  {- call them a and b -}
  a <- [2..99]
  b <- [2..a]

  let p0 (a, b) = a * b
  let s0 (a, b) = a + b

  {- there are two participants, p and s. -}
  {- p only knows the product of a and b -}
  {- s only knows the sum of a and b -}
  let p = p0 (a, b)
  let s = s0 (a, b)

  {- p says "I don't know a and b" -}
  let p1 = multiple_solutions . factors

  guard (p1 p)


  {- s says "I know you don't know" -}
  let s1 = all (p1 . p0) . components

  guard (s1 s)


  {- p now says "I know what the numbers are" -}
  let p2 = single_solution . filter (s1 . s0) . factors

  guard (p2 p)


  {- s finally says "I know what the numbers are" -}
  let s2 = single_solution . filter (p2 . p0) . components

  guard (s2 s)


  {- thus, the numbers are... -}
  return (a,b)

single_solution :: [a] -> Bool
single_solution [_] = True
single_solution _   = False

multiple_solutions :: [a] -> Bool
multiple_solutions = not . null . drop 1

factors :: Int -> [(Int, Int)]
factors n = do
  x <- [2..(floor (sqrt (fromIntegral n)))]

  guard ((n `mod` x) == 0)
  let y = n `div` x
  guard (y >= 2 && y <= 99)

  return (x, y)

components :: Int -> [(Int, Int)]
components n = do
  x <- [2..(n `div` 2) + 1]
  let y = n - x
  guard (y >= x && y >= 2 && y <= 99)
  return (x,y)
