isPrime :: Int -> Bool
isPrime inputNumber
    | elem (mod inputNumber 10) [2, 4, 5, 6, 8, 0] == True = False -- ignoring special cases of 2, 5 for this problem
    -- null is a function that takes a list and tells you whether or not the list is empty
    | otherwise = null $ filter (==0) (map (inputNumber `rem`) denominatorsList)
    where denominatorsList = [2..inputNumber-1]

largestPrimeFactor :: Int -> Int
largestPrimeFactor inputNumber =
    head $ filter isPrimeFactor [inputNumber, inputNumber-1..2]
    where isPrimeFactor n = inputNumber `rem` n == 0 && isPrime n

main :: IO ()
main = do
    -- HINT: do :set +s in ghci to enable timing and memory usage
    let result = largestPrimeFactor 18000001
    print result