{-# LANGUAGE RecordWildCards #-}
module ABPS where

import Control.Monad
import Data.List

solution = do
  {- there are two numbers each between 2 and 99 inclusive. -}
  {- call them a and b -}
  a <- [2..99]
  b <- [2..a]

  let p_0 a b = a * b
  let s_0 a b = a + b

  {- there are two participants, p and s. -}
  {- p only knows the product of a and b -}
  {- s only knows the sum of a and b -}
  let p = p_0 a b
  let s = s_0 a b

  {- p says "I don't know a and b" -}
  let p_1 p = do let p_ab's = factors p
                 guard (multiple_solutions p_ab's)
  p_1 p

  {- s says "I know you don't know" -}
  let s_1 s = do let ab_s = components s
                 let solutions = do (a,b) <- ab_s
                                    let p = p_0 a b
                                    p_1 p
                                    return (a,b)
                 guard (ab_s == solutions)
  s_1 s

  {- p now says "I know what the numbers are" -}
  let p_2 p = do let ab_s = factors p

                 let solutions = do (a,b) <- ab_s
                                    let s = s_0 a b
                                    s_1 s
                                    return (a,b)
                 guard (single_solution solutions)

  p_2 p

  {- s finally says "I know what the numbers are" -}
  let s_2 s = do let ab_s = components s

                 let solutions = do (a,b) <- ab_s
                                    let p = p_0 a b
                                    p_2 p
                                    return (a,b)
                 guard (single_solution solutions)

  s_2 s

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
  x <- [2..n]
  let y = n - x
  guard (y >= x && y >= 2 && y <= 99)
  return (x,y)
