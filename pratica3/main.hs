{- Ex 1
-- a)
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

(||) :: Bool -> Bool -> Bool
True || b = True
False || False = False

-- b)
ou :: Bool -> Bool -> Bool
ou x y = if x then True
              else if y then True
                        else False
-}

-- EX 2
dist :: (Float, Float) -> (Float, Float) -> Float
dist (x,y) (w,z) = sqrt((x-w)^2 + (y-z)^2) 

-- EX 3
fatg :: Int -> Int
fatg x 
     | x == 0 = 1
     | otherwise = x * fatg(x-1)

fatc :: Int -> Int
fatc 0 = 1
fatc x = x * fatc(x-1)

-- EX 4
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-2) + fibo(n-1)

-- EX 5
-- -- n = (n(n+1))/2
-- n_tri :: Int -> Int
-- n_tri 0 = 0
-- n_tri n = (n(n+1))/2

-- EX 6
potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 n = 2*potencia2(n-1)

-- EX 7
-- a)
prodIntervalo :: Int -> Int -> Int
prodIntervalo x y
     | y<x  = 0
     | x == y = y
     | otherwise = x * prodIntervalo (x+1) y

-- b)
fatPI :: Int -> Int
fatPI x = prodIntervalo 1 x

-- EX 8
resto_div :: Int -> Int -> Int -> Int
resto_div x y z = x - (z*y)

--z = 0
div_inteira :: Int-> Int ->Int -> Int
div_inteira x y z
     | x -y < 0 = z  
     | otherwise = div_inteira (x-y) y z+1  


-- EX 9 
     mdcg :: (Int, Int) -> Int
     mdcg (m,n)
          | n == 0 = m
          | otherwise = mdcg(n, (mod m n))

     mdcc :: (Int, Int) -> Int
     mdcc (m,0) = 0
     mdcc (m,n) =  mdcc(n, (mod m n))


-- 10
binomialg :: (Int, Int) -> Int
binomialg (n,k)
     | k == 0 = 1
     | k == n = 1
     |otherwise = binomialg(n-1,k) + binomialg(n-1,k-1)


binomialc :: (Int, Int) -> Int
binomialc (n,0) = 1
binomialc (n,n) = 1
binomialc (n,k) = binomialc(n-1,k) + binomialc(n-1,k-1)




               