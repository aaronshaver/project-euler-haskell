squareOfSums :: Int -> Int
squareOfSums x = sum [1..x] ^ 2

sumOfSquares :: Int -> Int
sumOfSquares x = sum (map (^2) [1..x])

main :: IO ()
main = do
    -- HINT: do :set +s in ghci to enable timing and memory usage
    let firstNNumbers = 100
    let result = squareOfSums firstNNumbers - sumOfSquares firstNNumbers
    print result