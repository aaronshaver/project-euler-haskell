numberUnderTest = 50113

isPrime :: Int -> Bool
isPrime x
    | x < 2 = False
    | x == 2 = True
    | otherwise =
    let denominatorsList = [2..x-1]
    in filter (==0) (map (x `rem`) denominatorsList) == []

dividesEvenly :: Int -> Bool
dividesEvenly denominator = do
    numberUnderTest `rem` denominator == 0

largestPrimeFactor :: Int -> Int
largestPrimeFactor x = do
    let checkForPrimality = [x,x-1..2]
    let primesList = filter (isPrime) checkForPrimality
    let isFactorList = filter (dividesEvenly) primesList
    head isFactorList

main :: IO ()
main = do
    let result = largestPrimeFactor numberUnderTest 
    print result