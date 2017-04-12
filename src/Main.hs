module Main where

import Data.Complex

main :: IO ()
main = do
  putStrLn "hello world"

maxIter :: Int
maxIter = 10000

-- data Scale = {
--                ULCorner :: Point
--                DRCorner :: Point
--                XScale :: Double
--                YScale :: Double
--              }

-- Simple boolean test to see if a point is in the set
isInSet :: RealFloat a => a -> a -> Bool
isInSet r i = case escapeTime r i of
              Nothing -> True
              Just _ -> False

-- Escape Time of Nothing means the point never escaped
escapeTime :: RealFloat a => a -> a -> Maybe Int
escapeTime r i
    | numIter == maxIter = Nothing
    | otherwise = Just numIter
    where numIter = mandel r i

-- Figures out how many iterations a point takes to escape
-- Or stops after maxIter iterations
mandel :: RealFloat a => a -> a -> Int
mandel r i = length . takeWhile ((<= 2) . magnitude) .
    take maxIter $ iterate (\z -> z^2 + (r :+ i)) 0
