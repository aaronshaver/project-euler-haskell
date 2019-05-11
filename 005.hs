evenDivision1to20 :: Int -> Bool
evenDivision1to20 x = null $ filter (\n -> x `mod` n /= 0) [1..20]

main :: IO ()
main = do
    -- HINT: do :set +s in ghci to enable timing and memory usage
    let result = head $ take 1 $ filter (evenDivision1to20) [10,20..]
    print result