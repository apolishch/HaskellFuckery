import Data.Ratio
euler3 :: Integer -> Integer -> [Integer] -> Integer
euler3 limit x []   
                 |(((floor(sqrt (fromIntegral limit :: Double)))^2) == limit) = (euler3 (floor(sqrt (fromIntegral limit :: Double))) x [])
                 |otherwise = euler3 limit 2 [0]
euler3 limit x (y:ys)  
                 |((((div limit x)%1) == (limit%x)) && (x<=(floor(sqrt (fromIntegral limit :: Double))))) = euler3 limit (x+1) (x:y:ys)
                 |(((((div limit x)%1) == (limit%x)) && (x>(floor(sqrt (fromIntegral limit :: Double)))) && (y/=0)) && ((euler3 x 0 [])== x)) = x
                 |((((div limit x)%1) == (limit%x)) && (x>(floor(sqrt (fromIntegral limit :: Double)))) && (y==0)) = limit
                 |(((div limit x)%1) /= (limit%x)) = euler3 limit (x+1) (y:ys)
                 |otherwise = max (euler3 (div limit x) 0 []) (euler3 x 0 [])
                 	         