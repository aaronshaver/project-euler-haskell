-- I got this from https://wiki.haskell.org/The_Fibonacci_sequence
-- The a+b is an accumulator argument which passes state to subsequent
-- calls of the function so that you don't need to re-compute earlier
-- calls, drastically increasing performance
fibonacci n = go n (0,1)
    where
        go n (a, b) | n == 0 = a
            | (a > 4000000) || (b > 4000000) = 0
            | otherwise = go (n-1) (b, a+b)

main :: IO ()
main = do
    let isEven x = x `mod` 2 == 0
    let result = sum (filter isEven (map (fibonacci) [0..40]))
    print result