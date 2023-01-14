absValue :: Int -> Int
absValue = abs

power :: Int -> Int -> Int
power a b = a^b

isPrime :: Int -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n = null divisors
    where divisors = filter ((== 0) . (n `mod`)) [2..round (sqrt (fromIntegral n)) + 1]

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 1) + slowFib (n - 2)

quickFib :: Integer -> Integer
quickFib = fst . fib2
    where   fib2 0 = (1, 1)
            fib2 1 = (1, 2)
            fib2 n
             | even n    = (a*a + b*b, c*c - a*a)
             | otherwise = (c*c - a*a, b*b + c*c)
             where (a,b) = fib2 (n `div` 2 - 1)
                   c     = a + b