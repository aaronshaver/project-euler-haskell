    putStrLn "enter a number to find highest prime factor: "
    input1 <- getLine
    let result = largestPrimeFactor (read input1 :: Int)