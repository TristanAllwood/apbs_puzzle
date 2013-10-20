{-# LANGUAGE DeriveDataTypeable #-}
module Viz where

import ABPS
import Data.Data

import Data.Aeson.Generic
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C

data Table
  = Table { titles :: [String]
          , rows   :: [[Value]]
          }
  deriving (Data, Typeable)

data Value = AandB (Int, Int)
           | Value Int
           | NestedTable Table
           | Several [Value]
           | Truth Bool
  deriving (Data, Typeable)

p_table abs
  = Table { titles = ["(a,b) are",   "p sees a*b", "p thinks (a,b) could be"]
          , rows   = [ [AandB (a,b), Value (a*b),    Several [AandB v | v <- p (a,b)] ]
                     | (a,b) <- abs
                     ]
          }

s_table abs
  = Table { titles = ["(a,b) are",   "s sees a+b", "s thinks (a,b) could be"]
          , rows   = [ [AandB (a,b), Value (a+b),    Several [AandB v | v <- s (a,b)] ]
                     | (a,b) <- abs
                     ]
          }

p1_table abs
  = Table { titles = titles pt ++ ["p sees multiple possibilities?"]
          , rows   = [ row ++ [Truth (p1 ab)] | row@(AandB ab:_) <- rows pt ]
          }
  where
    pt = p_table abs

s1_table abs
  = Table { titles = ["(a,b) are", "s sees a+b",
                      "s thinks what (a,b) could be and pretends to run p1",
                      "s knows p can only see multiple possibilities?"]
          , rows   = [ [ AandB (a,b), Value (a+b)
                       , NestedTable $ p1_table (s (a,b))
                       , Truth (s1 (a,b))
                       ]
                     | (a,b) <- abs
                     ]
          }

p2_table abs
  = Table { titles = [ "(a,b) are", "p sees a*b",
                       "p thinks what (a,b) could be and pretends to run s1",
                       "p knows s sees exactly one option?" ]
          , rows   = [ [ AandB (a,b), Value (a*b)
                       , NestedTable $ s1_table (p (a,b))
                       , Truth (p2 (a,b))
                       ]
                     | (a,b) <- abs
                     ]
          }

s2_table abs
  = Table { titles = [ "(a,b) are", "s sees a+b",
                       "s thinks what (a,b) could be and pretends to run p2",
                       "s knows p sees exactly one option?"]
          , rows   = [ [ AandB (a,b), Value (a+b)
                       , NestedTable $ p2_table (s (a,b))
                       , Truth (s2 (a,b))
                       ]
                     | (a,b) <- abs
                     ]
          }

main = do

  let all_abs = [ (a,b) | a <- [2..99], b <- [2..a] ]
  let interesting_abs = [(2,2), (6,5), (13,4)]

  L.writeFile "p.js" (C.pack "p_table = " `L.append` (encode $ p_table interesting_abs))
  L.writeFile "s.js" (C.pack "s_table = " `L.append` (encode $ s_table interesting_abs))

  L.writeFile "p1.js" (C.pack "p1_table = " `L.append` (encode $ p1_table interesting_abs))
  L.writeFile "s1.js" (C.pack "s1_table = " `L.append` (encode $ s1_table interesting_abs))

  L.writeFile "p2.js" (C.pack "p2_table = " `L.append` (encode $ p2_table interesting_abs))
  L.writeFile "s2.js" (C.pack "s2_table = " `L.append` (encode $ s2_table interesting_abs))
