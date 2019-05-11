{-
After I solved the puzzle myself, I stole this 11..20 trick from the PE thread, which
cuts the time exactly in half.

I think it works similar to this example for the smaller 1..6 set:

60
/6 = 10
/5 = 12
/4 = 15
/3 = 20 (if we can divide by 6, we can divide by 3)
/2 = 30 (if we can divide by 4, we can divide by 2)
/1 = 60 (don't need to check this for any number)
-}
evenDivision1to20 :: Int -> Bool
evenDivision1to20 x = null $ filter (\n -> x `mod` n /= 0) [11..20]

main :: IO ()
main = do
    -- HINT: do :set +s in ghci to enable timing and memory usage
    let result = head $ take 1 $ filter (evenDivision1to20) [10,20..]
    print result