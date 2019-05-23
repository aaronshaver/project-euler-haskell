-- I did my best to solve this on my own and did get a working but very slow solution.
-- (e.g. finding largest prime factor of 18,000,001 was taking about 8.5 seconds)
-- After that, I was stumped and looked for hints and translated this Python code to Haskell,
-- which was still a great learning opportunity! https://www.youtube.com/watch?v=Y3n-PA3QoAE

getFactors :: Int -> [Int]
getFactors inputNumber = do
    let highestPotentialFactor = round (sqrt (fromIntegral inputNumber))
    -- works because e.g. for 24 sqrt is 4.898.. and 5x5 is too big; we don't need to check the
    -- division of any number above that line because any above it will have a pair below the line
    -- saves us from having to check a bunch of numbers, e.g. for 24 we're not checking 7, 9, 10, 11, ...
    let firstHalf = filter (\n -> inputNumber `rem` n == 0) [1..highestPotentialFactor]
    -- finds the "pair" of that factor, i.e. that factor times the pair equals the target number:
    firstHalf ++ map (inputNumber `div`) firstHalf

isPrime :: Int -> Bool
isPrime inputNumber =
    length (getFactors inputNumber) == 2 -- list is [1,the number itself]

largestPrime :: [Int] -> Int -- input list assumed to be mix of prime, non-prime numbers
largestPrime numbers =
    last $ filter (isPrime) numbers

main :: IO ()
main = do
    -- HINT: do :set +s in ghci to enable timing and memory usage
    let result4 = largestPrime (getFactors 600851475143)
    print result4