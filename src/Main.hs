module Main where

import Data.Complex

main :: IO ()
main = do
  putStrLn "hello world"

iter :: Int
iter = 1000000

mandel :: RealFloat a => a -> a -> Int
mandel r i = length . takeWhile (\z -> magnitude z <= 2) .
    take iter $ iterate (\z -> z^2 + (r :+ i)) 0
