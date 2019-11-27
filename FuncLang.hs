module SomeFuncLangs where

    mix :: Ord a => [a] -> [a] -> [a]
    mix [] [a] = [a]
    mix [a] [] = [a]
    mix [] [] =[]
    mix (x:xs) (y:ys) | (x <= y) = x:y:mix xs ys
              |otherwise = y:x:mix xs ys 
    
    triplos :: [a] -> [(a,a,a)]
    triplos [] = []
    triplos (x:[]) = []
    triplos (x:y:[]) = []
    triplos (x:y:z:xs) = (x,y,z): triplos xs
                         
    
    fun :: Num a => [a] -> [a]
    fun l = map (*2) l
    
    fun1 :: Num a => [a] -> [a]
    fun1 [] = []
    fun1 (x:xs) = 2*x : fun1 xs
    
    type Filme = (Titulo,Realizador,[Actor],Ano,Duracao)
    type Titulo = String
    type Realizador = String
    type Actor = String
    type Ano = Int
    type Duracao = Int
    
    doActor :: Actor -> [Filme] -> [(Titulo,Ano)]
    doActor a [] = []
    doActor a ((t,r,x,y,d):ys) |((filtra a x) == True) = (t,y):doActor a ys 
                               |otherwise = doActor a ys
    
    filtra :: Actor -> [Actor] -> Bool
    filtra a [] = False
    filtra a (x:xs) | (a == x) = True
            | otherwise = filtra a xs
    
    
    total :: [Titulo] -> [Filme] -> Int
    total [] [a] = 0
    total [a] [] = 0
    total (a:xs) ((c,d,e,f,g):ys) | (a == c)= g + info a ys
                                  | otherwise = total xs ys
    
    
    info :: Titulo -> [Filme] -> Int
    info a [] = 0
    info a ((t,r,x,y,d):ys) | (a == t) = d + info a ys
                            |otherwise = info a ys 	 
    
    data Arvbin = Vazia
                  | Arv Int Arvbin Arvbin  
    -------------Map--------------
    fun3 :: [[Int]] -> [Int]
    fun3 l = [3 * x | y <- l ,x <- y]
    
    fun12 :: [Int] -> [Int]
    fun12 []= []
    fun12 l = map (3*) l
    
    ------------Filter------------
    
    mult3 [] = []
    mult3 l = filter (\x -> (mod x 3)== 0) l
    
    mult31 [] = []
    mult31 (x:xs) | ((mod x 3)== 0) = x:(mult31 xs)
              | otherwise = (mult31 xs)
    ---------------Take-----------
    
    take1 :: Int -> [Int] -> [Int]
    take1 n l = take n l
    
    -------------Drop------------
    
    drop1 :: Int -> [Int] -> [Int]
    drop1 n l = drop n l
    
    --------------TREES-------
    teste = (Arv 42 (Arv 35 (Arv 20 Vazia Vazia)
                (Arv 38	(Arv 37 Vazia Vazia)
                    (Arv 40 Vazia Vazia)))
            (Arv 49 Vazia
                (Arv 60 Vazia
                    (Arv 85 (Arv 70 Vazia Vazia)
                        (Arv 100 Vazia Vazia)))))
    
    somaArv :: Arvbin -> Int
    somaArv Vazia = 0
    somaArv (Arv x esq dir) = x + somaArv esq + somaArv dir
    
    trans :: Arvbin -> [Int]
    trans Vazia = []
    trans (Arv x esq dir) = [x] ++ trans esq ++ trans dir
    
    somatrans :: Arvbin -> Int
    somatrans Vazia = 0
    somatrans (Arv x esq dir) = sum (trans (Arv x esq dir))
    
    nelem :: Arvbin -> Int
    nelem Vazia = 0
    nelem (Arv x esq dir) = 1 + nelem esq + nelem dir
    
    peso :: Arvbin -> Int
    peso Vazia = 0
    peso (Arv x esq dir) = div (length(trans (Arv x esq dir))) 2
    
    insere :: Int -> Arvbin -> Arvbin
    insere n Vazia = (Arv n Vazia Vazia)
    insere n (Arv x esq dir) | (n == x) = (Arv x esq dir)
                             | (n > x) = Arv x (insere n esq) dir
                             | (n < x) = Arv x esq (insere n dir)
    
    --data Maybe a = Nothing | Just a 
    
    divisao :: Int -> Int -> Maybe Int
    divisao _ 0 = Nothing
    divisao n d = Just (div n d)
    