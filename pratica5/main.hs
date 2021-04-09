--EX 1

conta_ch :: [Char] -> Int
conta_ch [] = 0
conta_ch (x:xs) = 1 + conta_ch xs


conta :: [t] -> Int
conta [] = 0
conta (x:xs) = 1 + conta xs

maior :: [Int] -> Int
maior [x] = x
maior (x:y:xs)
     | x > y = maior (x:xs)
     | otherwise = maior (y:xs)

primeiros :: Int -> [a] -> [a]
primeiros _ [] = []
primeiros 0 _ = []
primeiros n (x:xs) = x:primeiros (n-1) xs


pertence:: Eq a => a -> [a] -> Bool
pertence n [] = False
pertence n (x:xs) =
     if(n == x) then True
                else pertence n xs

uniaoR :: Eq t => [t] -> [t] -> [t]
uniaoR [] lst = lst
uniaoR (x:xs) lst =
     if pertence x lst
          then uniaoR xs lst
          else x: uniaoR xs lst


-- EX 2

npares:: [Int] -> Int
npares [] = 0
npares (x:xs) 
     | mod x 2 == 0 = 1 + npares xs
     | otherwise = npares xs

-- EX 3
produto :: [Int] -> Int
produto [] = 1
produto (x:xs) = x * produto xs

--EX 4
-- comprime :: [[Int]] -> [Int]
-- comprime (x:xs) = comprime x:comprime xs

-- EX 5
tamanho :: [t] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

-- EX 6
uniaoRec2 :: [Int] -> [Int] -> [Int]
uniaoRec2 lst [] = lst
uniaoRec2 lst (y:ys) = 
     if pertence y lst
          then uniaoRec2 lst ys 
          else y : uniaoRec2 lst ys

