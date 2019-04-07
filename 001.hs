main :: IO ()
main = do
    let multipleThreeOrFive x = (x `mod` 3 == 0) || (x `mod` 5 == 0)
    let result = sum (takeWhile (<1000) (filter multipleThreeOrFive [1..]))
    print result