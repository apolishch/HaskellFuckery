euler1 limit (divisor:[]) = if ((limit > 1) && ((mod limit divisor) == 0)) then limit else 0
euler1 limit (div:divisors) 
                          | ((limit > 1) && (euler1 limit (div:[]))==0) = (euler1 limit divisors) + (euler1 (limit-1) (div:divisors)) 
                          | ((limit > 1) && (euler1 limit (div:[]))/=0) = (euler1 limit (div:[])) + (euler1 (limit-1) (div:divisors)) 
                          | limit > 1 = euler1 (limit-1) (div:divisors) 
                          | otherwise = 0
euler1 limit [] = 0