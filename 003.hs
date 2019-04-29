isPrime :: Int -> Bool
isPrime x
    | x < 2 = False
    | x == 2 = True
    | otherwise =
    let denominatorsList = [2..x-1]
    in filter (==0) (map (x `rem`) denominatorsList) == []

largestPrimeFactor :: Int -> Int
largestPrimeFactor x = do
    let checkForPrimality = [x,x-1..2]
    let primesList = filter (isPrime ) checkForPrimality
    head (filter (\n -> x `rem` n == 0) primesList)

main :: IO ()
main = do
    putStrLn "enter a number to find highest prime factor: "
    input1 <- getLine
    let numberUnderTest = read input1 :: Int
    let result = largestPrimeFactor numberUnderTest 
    print result