fib :: Integer -> Integer

fib (x)
    | x==0 = 1
    | x==1 = 1
    | x>1 = fib(x-1) + fib(x-2)