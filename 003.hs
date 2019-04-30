isPrime :: Int -> Bool
isPrime inputNumber
    | inputNumber < 2 = False
    | inputNumber == 2 = True
    | otherwise =
    let denominatorsList = [2..inputNumber-1]
    in filter (==0) (map (inputNumber `rem`) denominatorsList) == []

largestPrimeFactor :: Int -> Int
largestPrimeFactor inputNumber = do
    -- note: will output the input number itself if it's prime!
    let primesList = filter (isPrime) [inputNumber,inputNumber-1..2]
    head (filter (\n -> inputNumber `rem` n == 0) primesList)

main :: IO ()
main = do
    putStrLn "enter a number to find highest prime factor: "
    input1 <- getLine
    let result = largestPrimeFactor (read input1 :: Int)
    print result