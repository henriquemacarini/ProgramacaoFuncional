-- EX 2
geraa = [5,4..1]
gerab = ['a','c'..'e']
gerac = [1,4..16]
gerad = zip [1,(-2)..(-11)] [1,5..17]

-- EX 3
-- a)
geraL a b 
     | b > a = [a..b]
     | a == b = [a]
     | otherwise = []

--b)

geraLP a b 
     | b > a && even a = [a, a+2..b]
     | b > a && odd a = [a+1, a+3..b]
     | otherwise = []

-- EX 5
quadrado x y = [x^2 | x <- [x..y]]

-- EX 6
seleciona_impares y = [x | x <- y , odd x]

--EX 7
tabuada x = [ x*y | y <- [1..10]]

--EX 8
bissexto :: Int->Bool
bissexto x
     | (mod x 400) == 0 = True
     | (mod x 4 == 0 && mod x 100 /= 0) = True
     | otherwise = False

bissextos x = [ y | y <- x, bissexto y]

--EX 9
sublista x = [ y | y <- x, head y, tail y]

-- EX 10