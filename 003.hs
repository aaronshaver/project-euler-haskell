isPrime :: Int -> Bool
isPrime inputNumber
    -- ignoring special cases of 2, 5 for this problem
    | elem (mod inputNumber 10) [2, 4, 5, 6, 8, 0] == True = False
    | otherwise =
    let denominatorsList = [2..inputNumber-1]
    in filter (==0) (map (inputNumber `rem`) denominatorsList) == []

largestPrimeFactor :: Int -> Int
largestPrimeFactor inputNumber = do
    head (filter (\n -> inputNumber `rem` n == 0 && isPrime n == True) [inputNumber, inputNumber-1..2])

main :: IO ()
main = do
    -- HINT: do :set +s in ghci to enable timing and memory usage
    let result = largestPrimeFactor 18000001
    print result