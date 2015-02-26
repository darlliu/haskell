--a strategy based parallel/concurrent filter and sorter
import Control.Monad
import Data.List
import Control.Parallel.Strategies
{-module Main() where-}

f:: Int -> Float
f x = if x > 1 then 1/(fromIntegral x)*(f (x-1))
        else 1
main = do
    let arr = take 40000 [1..]
    let arr' =  parMap rseq f arr
    let sum = foldl1' (+) arr'
    putStrLn $ "The sum of reciporal is "++ (show sum)
