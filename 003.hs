generatePrimes x = x : generatePrimes (x + 1)

main :: IO ()
main = do
    let result = takeWhile (<= 50) (generatePrimes 1)
    print result