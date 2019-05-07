-- borrowed from https://stackoverflow.com/a/3963286
digits :: Integral x => x -> [x]
digits 0 = [] -- prevents infinite loop of calling "digits 0" over and over
digits x = digits (x `div` 10) ++ [x `mod` 10]

isPalindrome :: Int -> Bool
isPalindrome inputNumber =
    reverse (digits inputNumber) == digits inputNumber

main :: IO ()
main = do
    -- HINT: do :set +s in ghci to enable timing and memory usage
    let result = maximum $ take 100 $ filter (isPalindrome) $ [x * y | x <- [999,998..100], y <- [999,998..100]]
    print result