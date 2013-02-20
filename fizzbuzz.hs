--fizzbuzz no arithmetics
f (x:y:z:rest)=x:y:"Fizz":f rest
f xs = xs

g (x1:x2:x3:x4:"Fizz":rest) = x1:x2:x3:x4:("FizzBuzz"):g rest
g (x1:x2:x3:x4:x5:rest) = x1:x2:x3:x4:"Buzz":g rest
g xs = xs

fizzbuzz num
    |num== 0 = ["0"]
    |otherwise = "0" : (g $ f $ map show $ takeWhile(<num) [1..])
