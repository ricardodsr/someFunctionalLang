module Sudoku where

    import List
    import Monad
    import Char
    
    --Tipos sinonimos utilizados
    
    type EstadoSudoku = [[Int]]
    type JogoSudoku a = Jogo EstadoSudoku a
    type Posicao = (Int,Int)
    type PosicaoCaixa = (Int,Int)
    type Candidatos = [Int]
    type Porpreencher = (Posicao,Candidatos)
    type Jogada = (Posicao,Int)
    
    --Defeni�ao de Monade a utilizar e respetivas funcoes de tranformacao
    
    data Jogo s a = Jogo (s -> Maybe (s,a)) 
    
    instance Monad (Jogo a) where
        return a 	= Jogo (\s -> Just (s,a))
        (Jogo g) >>= f 	= Jogo (\s0 -> case (g s0) of
                        Nothing -> Nothing
                        Just (s1, a) -> let (Jogo h) = f a in h s1)
    
    falhou = Jogo (\_ -> Nothing)
    
    instance MonadPlus (Jogo a) where
        mzero = falhou
        mplus (Jogo p) (Jogo q) = Jogo( \s0 -> case p s0 of
                            Just (s1,a) -> Just (s1,a)
                            Nothing -> q s0 )
    
    
    jogarM :: Jogo a b -> a -> Maybe (a, b)
    jogarM (Jogo a) = a
    
    
    desfaz1 :: Maybe (EstadoSudoku,a) -> a
    desfaz1 x = case x of
            Just (_,s) -> s
    
    desfaz :: Maybe (a,b) -> a
    desfaz x = case x of
            Just (s,_) -> s
    
    --varias listas auxiliares para mais facil resolu�ao
    todasLinhas::[Int]
    todasLinhas = [0..8]
    
    todasColunas::[Int]
    todasColunas = [0..8]
    
    todasCaixas ::[Int]
    todasCaixas = [0..2]
    
    --listas definidas por compreensao que nos dao todas as posicoes
    todasPosicoes :: [Posicao]
    todasPosicoes = [(x,y) | x <- todasColunas, y <- todasLinhas]
    
    todasCaixasPosicao :: [PosicaoCaixa]
    todasCaixasPosicao = [(x,y) | x <- todasCaixas , y <- todasCaixas]
    
    posicoesCaixa :: PosicaoCaixa -> [Posicao]
    posicoesCaixa (x,y) = let
                    xs = x*3
                          ys = y*3
                  in	[(x,y) | x <- [xs..xs+2] , y <- [ys..ys+2]]
    
    
    
    --coloca no estado e no elemento auxiliar
    selecionaLinhaAux :: JogoSudoku [[Int]]
    selecionaLinhaAux = Jogo (\est -> Just (est,est))
    
    --varias fun�oes de seleccao de colunas, linhas e caixas
    selecionaLinha :: Int -> JogoSudoku [Int]
    selecionaLinha x = do	s <- selecionaLinhaAux
                return (s !! x)-- retira da lista s o elemento x
    
    
    selecionaColuna :: JogoSudoku [[Int]]
    selecionaColuna = mapM selecionaColunaAux todasColunas
    
    selecionaColunaAux :: Int -> JogoSudoku [Int]
    selecionaColunaAux x = do s <- selecionaLinhaAux
                  return [n!!x | n <- s]
    
    selecionaCaixa :: JogoSudoku [[Int]]
    selecionaCaixa = mapM selecionaCaixaAux todasCaixasPosicao
    
    selecionaCaixaAux :: PosicaoCaixa -> JogoSudoku [Int]
    selecionaCaixaAux x = mapM conteudo (posicoesCaixa x)
    
    --da o valor de uma determinada posi�ao
    conteudo :: Posicao -> JogoSudoku Int
    conteudo (x , y) = do 
            s <- selecionaLinhaAux 
            return ((s !! y) !! x)
    
    novoJogo :: [[Int]]
    novoJogo = [[0 | x <- [0..8]]|y <- [0..8]]
    
    
    --FORMA DE RESOLU�AO
    
    --Funcao k mete a grelha no estado
    meteGrelha :: [[Int]] -> JogoSudoku ()
    meteGrelha grelha = Jogo (\_ -> Just (grelha, ()))
    
    --funcao k faz uma jogada
    jogada :: Jogada -> JogoSudoku ()
    jogada x = do 
             s <- selecionaLinhaAux
             let actualizaGrelha [] x y = []
             actualizaGrelha (z:zs) x y = (actualizaCol z x 0 y) : (actualizaGrelha zs x (y + 1)) 
                    actualizaCol [] x h y = []
             actualizaCol (w:ws) x h y = (actualizaColAux w x h y) : (actualizaCol ws x (h + 1) y)
                   actualizaColAux b x@((ax,ay), a) bx by = if (ax,ay)==(bx,by) then a else b 
             meteGrelha (actualizaGrelha s x 0)
    
    --funcao k desfaz uma jogada(apenas utilizada no IO que permite o utilizador jogar)
    undo :: Jogada -> JogoSudoku ()
    undo x = do 
             s <- selecionaLinhaAux
             let actualizaGrelha [] x y = []
             actualizaGrelha (z:zs) x y = (actualizaCol z x 0 y) : (actualizaGrelha zs x (y + 1)) 
             actualizaCol [] x h y = []
             actualizaCol (w:ws) x h y = (actualizaColAux w x h y) : (actualizaCol ws x (h + 1) y)
                   actualizaColAux b x@((ax,ay), a) bx by = if (ax,ay)==(bx,by) then 0 else b
             meteGrelha (actualizaGrelha s x 0)
    
    --mapeamento da funcao "jogada" a uma lista de jogadas ( o "_" foi usado para compatibilidade dos tipos)
    actualizar :: [Jogada] -> JogoSudoku ()
    actualizar = mapM_ jogada
    
    --algarismos k faltam numa caixa coluna ou linha
    algarismosMissing :: [Int]->[Int]		
    algarismosMissing x = [1..9] \\ x
    
    --algarismos contidos numa caixa
    contidoCaixa :: Posicao -> PosicaoCaixa
    contidoCaixa (x,y) = (div x 3 ,div y 3)
    
    --verifica se uma posicao esta vazia
    verificaVazio :: Posicao -> JogoSudoku Bool
    verificaVazio x = liftM (==0) (conteudo x)
    
    --da a coordenadas das posi�oes vazias
    posicoesVazias :: JogoSudoku [Posicao]
    posicoesVazias = filterM verificaVazio todasPosicoes
    
    --adiciona as posicoes vazias a sua respectiva lista de candidatos
    porpreencher:: JogoSudoku[Porpreencher]
    porpreencher = do pV <- posicoesVazias
              mapM (\pos -> do s <- candidatos pos; return (pos,s)) pV
    
    --obtem os candidatos para uma determinada posicao
    candidatos :: Posicao -> JogoSudoku [Int]
    candidatos cord@(x,y) = do
            l <- selecionaLinha y
            c <- selecionaColunaAux x
            caixa <- selecionaCaixaAux (contidoCaixa cord)
            return ((algarismosMissing l) `intersect` (algarismosMissing c) `intersect` (algarismosMissing caixa))
    
    --verifiva se a lista e unitaria
    unico [] = False
    unico (x:xs) = null xs
    
    --selecioana as jogadas que so tem um candidato
    selecionaJogadasForcadas :: JogoSudoku [Jogada]
    selecionaJogadasForcadas = do eV <- porpreencher
                      let eF = filter (\(cord,x) -> unico x) eV
                      return [(cord,v) | (cord, (v:_)) <- eF]
    
    --funcao que faz as jogadas for�adas
    fazJogadas :: JogoSudoku ()
    fazJogadas = do jF <- selecionaJogadasForcadas
            if null jF
                then return ()
                else actualizar jF >> fazJogadas
    
    --fun�ao identica a anterior mas so usada no IO para jogar
    simplify :: JogoSudoku [Jogada]
    simplify = do jF <- selecionaJogadasForcadas
              if null jF
                then return []
                else do actualizar jF
                    return jF
    
    --verica se ha valores repetidos em todas as linhas colunas e caixas, ou seja se um tabuleiro esta valido no momento
    evalido :: JogoSudoku Bool
    evalido = do l <- selecionaLinhaAux
             c <- selecionaColuna
             caixa <-selecionaCaixa
             return ((todosvalidos l) && (todosvalidos c) && (todosvalidos caixa))
    
    --uma especie de mapeamento mas usando o foldr
    todosvalidos = foldr (\x b -> b && (valoresValidos x)) True
    
    --verifica se ha valores repetidos numa linha coluna ao caixa
    valoresValidos [] = True
    valoresValidos (x:xs) = (if x /=0 then (not (x `elem` xs)) else True) && (valoresValidos xs)
    
    --funcao k da lista de posicoes por preecher nos da as jogadas possiveis da que tem menos candidatos
    proximo :: [Porpreencher] -> [Jogada]
    proximo x = let (e:_) = sortBy (\(_,cs1) (_,cs2) -> compare (length cs1) (length cs2)) x--da o primeiro elemento da lista ordenada por numero de candidatos
            (pos , cs) = e
            in [(pos,c) | c <- cs]--da a lista de jogadas possiveis para essa mesma posi�ao
    
    
    --analisa se ha ou nao uma resolucao
    fazAnalise :: JogoSudoku [Jogada]
    fazAnalise = do eV <- porpreencher
            let c = filter (\(_,xs) -> null xs) eV
            if null eV--verificar se ha posicoes por preencher
                then return []
                else if not (null c)--verificar se ha posi�oes sem alerntivas
                    then falhou
                    else return (proximo eV)
    
    --funcao que recebe uma lista de jogadas possiveis e tenta realiza-las todas
    tentaTudo :: [Jogada] -> JogoSudoku ()
    tentaTudo jog = msum [jogada m >> resolve | m <- jog]--msum t
            
    resolve:: JogoSudoku ()
    resolve = do fazJogadas
             v <- evalido
             if v
            then do jog <- fazAnalise
                if not (null jog)
                    then tentaTudo jog
                    else return ()
            else falhou
    
    
    jogar :: [Jogada] -> JogoSudoku ()
    jogar x = actualizar x >> resolve
    
    --fun�oes de formatacao
    
    mostra::EstadoSudoku -> IO ()
    mostra l = putStrLn "-------------------------------------" >> mapM_ mostraLinha l >> putStr "\n"
    
    mostraLinha p = putStr "|" >> mapM_ mostraPos p >> putStr "\n">>putStrLn "-------------------------------------"
                                          
    mostraPos 0 = putStr "   |"
    mostraPos a = putStr (" "++(show a)++" |")
    
    
    mostra1::[Porpreencher] -> IO ()
    mostra1 l = mapM_ mostraposcan l >> putStr "\n"
    
    mostraposcan ((a,b),c) = putStrLn ("("++(show a)++","++(show b)++")"++"\t"++(show c)) 
    
    --Leitura e respectivas fun�oes auxiliares de transforma�ao
    
    transformalistaaux :: ((Int,Int),Int) -> (Int,Int)
    transformalistaaux ((a,b),c) = (a,b)
    
    transformalista :: [((Int,Int),Int)] -> [(Int,Int)]
    transformalista x = map transformalistaaux x
    
    transforma1 :: Int -> [Int] -> [(Int,Int)]
    transforma1 _ [] = []
    transforma1 x (y:ys) = (x,y):(transforma1 (x+1) ys)
    
    transforma3 :: Int -> [(Int,Int)] -> [Jogada]
    transforma3 _ [] = []
    transforma3 x ((a,b):xs) = (((a,x),b): transforma3 x xs)
    
    transforma :: Int -> [[Int]] -> [[Jogada]] 
    transforma _ [] = []
    transforma y (x:xs) =(transforma3 y (transforma1 0 x): transforma (y+1) xs)
    
    transformafinal :: [[Int]] ->[Jogada]
    transformafinal l = concat (transforma 0 l)
    
    readaux :: Char -> Int
    readaux x = if (x==' ') then 0
                  else (ord x)-48
    
    
    readaux1 :: [Char] -> [Int]
    readaux1 l = map readaux l
    
    readaux2 :: [Jogada] -> [Jogada]
    readaux2 [] = []
    readaux2 (((a,b),c):xs) = if (c==0) then readaux2 xs
                        else ((a,b),c) : readaux2 xs
    
    
    meuread x = do s <- readFile (x)
               let l = lines s
                   h = map readaux1 l
                   j = transformafinal h
                   i = readaux2 j
               return i
    
    
    -- IO que permite ao utilizador jogar
    jogo s1 jog x = do   a <- lerOpcao "1234567890"
                 case a of
                '1' -> do	b <- meuread x
                        let s2 = jogarM (actualizar b) novoJogo
                        case s2 of
                            Nothing -> do putStrLn "Falhou Carregamento"
                                      jogo s1 jog x
                            Just (s,_) -> do putStrLn "Carregamento feito com Sucesso"
                                     mostra s 
                                     jogo (desfaz s2) jog x
                
                '2' -> do	case jogarM (actualizar []) s1 of
                            Nothing -> putStr " Falhou"
                            Just (s,_) -> mostra s
                        jogo s1 jog x
    
                '3' -> do 	inicial <- meuread x
                        putStrLn "Escreva a Coord de x"
                        a <- lernumero ["0","1","2","3","4","5","6","7","8"]
                          putStrLn "Escreva a Coord de y"
                        b <- lernumero ["0","1","2","3","4","5","6","7","8"]
                          putStrLn "Escreva o valor a introduzir"
                        c <- lernumero ["1","2","3","4","5","6","7","8","9"]
                          let a1 = (read a) :: Int
                                  a2 = (read b) :: Int
                                  a3 = (read c) :: Int
                            x1 = ((a1,a2),a3)
                            x2 = (a1,a2)
                            b1 = transformalista inicial	
                        if (elem x2 b1) then do putStrLn ("Jogada invalida: coordenadas de uma posicao inalteravel")
                                    jogo s1 jog x
                                else case (jogarM (actualizar [x1] >> evalido) s1) of
                                        Nothing -> putStrLn "Falhou"
                                        Just (_,True) -> do 	let s2 = jogarM (actualizar [x1]) s1
                                                    jogo (desfaz s2) (x1:jog) x
                                        Just (_,False)-> do	putStrLn ("Jogada invalida: valor invalido para esta posicao")
                                                    jogo s1 jog x
                    
                '4' -> do 	if jog/=[] then do
                                    let 	y1 = reverse (take ((length jog) - 1) (reverse jog)) 
                                                  x1 = (head jog)
                                              s2 = jogarM (undo x1) s1
                                      jogo (desfaz s2) y1 x
                               else do putStrLn "Nao ha mais jogadas para anular"
                                   jogo s1 jog x
    
                '5' -> do	inicial <- meuread x
                        putStrLn "Escreva a Coord de x"
                        a <- lernumero ["0","1","2","3","4","5","6","7","8"]
                          putStrLn "Escreva a Coord de y"
                        b <- lernumero ["0","1","2","3","4","5","6","7","8"]
                        let a1 = (read a) :: Int
                                  a2 = (read b) :: Int
                            x1 = (a1,a2)
                            x2 = desfaz1 (jogarM (candidatos x1) s1)
                            b1 = transformalista inicial
                        if (elem x1 b1) then do putStrLn ("Posicao invalida: coordenadas de uma posicao inalteravel")
                                    jogo s1 jog x
                                else do print x2
                                    jogo s1 jog x
    
                '6' -> 		do let l = (desfaz1 (jogarM (porpreencher) s1))
                           mostra1 l
                           jogo s1 jog x
                '7' ->		do let s2 = jogarM (simplify) s1
                           jogo (desfaz s2) ((desfaz1 s2)++jog) x
                '8' ->		do putStrLn "Escreva a Coord de x"
                           x1 <- lernumero ["0","1","2","3","4","5","6","7","8"]
                           putStrLn "Escreva a Coord de y"
                           y1 <- lernumero ["0","1","2","3","4","5","6","7","8"]
                           let a1 = (read x1) :: Int
                                     a2 = (read y1) :: Int
                               a = desfaz1 (jogarM (resolve >> conteudo (a1,a2)) s1) 
                           putStrLn ("A sulocao de posicao e : "++(show a))
                           jogo s1 jog x
    
                '9' ->		do case jogarM (resolve) s1 of
                            Nothing -> do putStrLn "Falhou"
                                      jogo s1 jog x
                            Just (g,_) -> do mostra g
                                     jogo s1 jog x
                '0' ->		do menu	
                otherwise ->    do putStrLn "Opcao invalida"
                           jogo s1 jog x
    
    
    --Menus e apresenta�ao do programa
    
    menu  =  do {putStr menuPrincipal;
         x <- lerOpcao "120";
         case x of
         '1' -> do putStrLn "Escreva o nome do ficheiro:"
                   x <- getLine
               putStr menuJogo
               jogo novoJogo [] x
               
             '2' -> do {putStr menuAutores;
                    a <- lerOpcao "120";
                    case a of
                    '1' -> do menuNome1
                    '2' -> do menuEmail1 
                    otherwise -> menu
                       }
         otherwise -> return ()  	
           }
    
    
    lernumero range = do x <-lerStr "valor: "
                 if (elem x range)
                     then return x
                     else do  putStr "valor invalido\n"
                         lernumero range
                                  
    
    lerOpcao range = do x <-lerStr "\nEscolha a opcao: ";
                if x/= [] then if (elem (head x)range)
                             then return (head x)
                             else do putStr "Opcao invalida\n"
                                  lerOpcao range
                      else do lerOpcao range  
                         
    lerOpcao1 range = do x <-lerStr "\nPrima 0 para voltar: ";
                 if x/= [] then if (elem (head x)range)
                             then return (head x)
                             else do putStr "Opcao invalida\n"
                                  lerOpcao range
                       else do lerOpcao range  
    
    
    menuNome1:: IO ()
    menuNome1 = do {putStr menuNome;
                    a <- lerOpcao1 "0";
                    (case a of
                    '0'-> menu
                    otherwise -> menu
                    )
                   }
    
    menuEmail1:: IO ()
    menuEmail1 = do {putStr menuEmail;
                    a <- lerOpcao1 "0";
                    (case a of
                    '0'-> menu
                    otherwise -> menu
                    )
                   }
    lerStr msg = do putStr msg 
                getLine
    
    menuPrincipal = " -----------------------Menu Principal------------------------- \n " ++
                    "|Novo Jogo....................................................1|\n " ++
            "|Autores......................................................2|\n " ++
            "|                                                              |\n " ++
            "|Terminar.....................................................0|\n " ++
                    " ------------------------------------------------------------- \n\n\n"
    
    menuJogo =      " ------------------------Menu de Jogo--------------------------- \n " ++
                    "|Carregar......................................................1|\n " ++
            "|Mostra Estado.................................................2|\n " ++
            "|Faz Jogada....................................................3|\n " ++
            "|Undo..........................................................4|\n " ++
            "|Alternativas para uma posicao.................................5|\n " ++
                    "|Alterntivas para todas as posicoes por prencher...............6|\n " ++
            "|Simplifica....................................................7|\n " ++
            "|Auto-solve Posicao............................................8|\n " ++
            "|Auto-solve Tudo...............................................9|\n " ++
            "|...............................................................|\n " ++
            "|...............................................................|\n " ++
            "|                                                               |\n " ++
            "|Menu..........................................................0|\n " ++
                    " --------------------------------------------------------------- \n\n\n"
    
    menuAutores =  " -----------------------------Autores----------------------------\n "  ++
                   "|Nomes dos autores..............................................1|\n " ++
                   "|Contactos......................................................2|\n " ++
                   "|                                                                |\n " ++
               "|Voltar ao menu Principal.......................................0|\n " ++
                   " ----------------------------------------------------------------\n\n\n "
    
    menuNome =     " ---------------Autores deste trabalho---------------------------\n "  ++
                   "|Nome: Ricardo Dovichi de Sousa Rouco............................|\n " ++
                   "|Nome: Pedro de Araujo Goncalves.................................|\n " ++
               "|                                                                |\n " ++
                   " ----------------------------------------------------------------\n\n\n "
            
    menuEmail =    "  -----------------------Email para contactos-------------------- \n " ++
                   "|Nome: Ricardo ....................Email: ricardo.rouco@gmail.com|\n " ++
                   "|Nome: Pedro.....................Email: p_miranda@portugalmail.pt|\n " ++
               "|                                                                |\n " ++
                   " ----------------------------------------------------------------\n\n\n " 
    
    