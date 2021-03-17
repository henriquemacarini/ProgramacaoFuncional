dobro :: Int->Int
dobro x = x * 2

quadruplicar :: Int->Int
quadruplicar x = dobro (dobro x)

hipotenusa :: Float->Float->Float
hipotenusa x y = sqrt( (x^2) + (y^2))


distancia :: Float->Float->Float->Float->Float
distancia x y w z = sqrt((x+w)^2 + (y+z)^2)

conversao :: Float->(Float,Float,Float)
conversao x = (x, 3.90*x, 4.45*x)

bissexto :: Int->Bool
bissexto x
     | (mod x 400) == 0 = True
     | (mod x 4 == 0 && mod x 100 /= 0) = True
     | otherwise = False

type Data = (Int, Int, Int)
bissexto2 :: Data->Bool
bissexto2(d,m,a) 
     | d>=1 && d<=31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) = True
     | d>=1 && d<=30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
     | d>=1 && d<=28 && m ==2 && not (bissexto a) = True
     | d>=1 && d<=29 && m ==2 && (bissexto a) = True
     | otherwise = False

valida :: Data->Bool
valida(d,m,a)
     | a < 0 = False
     | d>=1 && d<=31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) = True
     | d>=1 && d<=30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
     | d>=1 && d<=28 && m ==2 && not (bissexto a) = True
     | d>=1 && d<=29 && m ==2 && (bissexto a) = True
     | otherwise = False

precede :: Data->Data->Bool
precede (d1,m1,a1) (d2,m2,a2) 
     | a1 < a2 = True
     | a1 == a2 && m1 < m2 = True
     | a1 == a2 && m1 == m2 && d1 < d2 = True
     | otherwise = False

type Livro = (String,String,String,String,Int)
type Aluno = (String,String,String,String)
type Emprestimo = (String,String,Data,Data,String)

verificaEmprestimo :: Emprestimo->Bool
verificaEmprestimo (codL, codA, (dE, mE, aE), (dD, mD, aD), sit) =
     if (precede(dE, mE, aE) (dD, mD, aD)) then True
                                           else False

     