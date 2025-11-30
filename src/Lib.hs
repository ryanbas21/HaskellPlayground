module Lib
  ( tripleList,
  )
where

{- Triple a  list
>>> tripleList [1]
[1,1,1]
-}

tripleList :: [a] -> [a]
tripleList a = a ++ a ++ a
