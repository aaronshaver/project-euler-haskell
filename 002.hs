fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

main :: IO ()
main = do
    let isEven x = x `mod` 2 == 0
    let result = sum (filter isEven (map (fibonacci) [0..29]))
    print result