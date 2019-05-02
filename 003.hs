isPrime :: Int -> Bool
isPrime inputNumber
    | inputNumber < 2 = False
    | inputNumber == 2 = True
    | otherwise =
    let denominatorsList = [2..inputNumber-1]
    -- divide input by each denominator from 2 to N-1
    -- then filter that list for any zeros (lack of remainders)
    -- then return a bool of whether that filtered list is empty
    in filter (==0) (map (inputNumber `rem`) denominatorsList) == []

largestPrimeFactor :: Int -> Int
largestPrimeFactor inputNumber = do
    -- note: will output the input number itself if it's prime!
    head (filter (\n -> inputNumber `rem` n == 0 && isPrime n == True) [inputNumber,inputNumber-1..2])

main :: IO ()
main = do
    -- HINT: do :set +s in ghci to enable timing and memory usage
    let result = largestPrimeFactor 50011
    print result