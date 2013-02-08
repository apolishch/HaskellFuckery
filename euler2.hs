euler2 limit (num1:num2) |(num1>limit) = 0
                         |((num2>limit) && ((mod num1 2) == 0)) = num1
                         |(num2>limit) = 0
                         |(((mod num1 2) == 0) && ((mod num2 2) == 0)) = num1+num2+(euler2 limit [num1+num2,num1+num2+num2])
                         |((mod num1 2) == 0) = num1+(euler2 limit [num1+num2,num1+num2+num2])
                         |((mod num2 2) == 0) = num2+(euler2 limit [num1+num2,num1+num2+num2])
                         |otherwise = euler2 limit [num1+num2,num1+num2+num2]
euler2 limit [] = euler2 limit [1,2]