{-- Batalha Naval --}
import System.Random -- cabal install random
import Control.Concurrent
import System.Console.ANSI -- cabal install ansi-terminal
import Data.List
import System.Exit
import System.IO

{--
    Símbolos:
    "~" -> Água
    "#" -> Navio
    "o" -> Água sem navio
    "X" -> Navio atingido
--}

main :: IO()
main = do
    clearScreen
    putStrLn "Bem-vindo a Batalha Naval!"
    putStrLn "O que voce ira fazer?"
    putStrLn "(n) - Novo Jogo"
    putStrLn "(c) - Carregar Jogo"
    putStrLn "(s) - Sair do Jogo"

    opcao <- getLine

    case opcao of {
        "n" -> novoJogo;
        "c" -> carregarJogo;
        "s" -> die "Fechando jogo...";
        _ -> do
            putStrLn "Opcao invalida!"
            main
    }

novoJogo :: IO()
novoJogo = do
    tab_j <- montaTabuleiro ""
    tab_jogador_ve_bot <- montaTabuleiro ""
    tab_bot <- montaTabuleiro "B"
    tab_bot_ve_jogador <- montaTabuleiro ""

    clearScreen
    -- TODO: Remover este print na versão final
    printaTabEMensagem tab_bot "Este eh o tabuleiro do bot, nao fala pra ninguem que voce viu..."

    threadDelay 2000000

    clearScreen 
    putStrLn "Hora da preparacao, escolha as posicoes de seus navios!\n\n"

    -- Pede ao jogador que posicione os 5 navios
    threadDelay 2500000
    clearScreen
    tab_j1 <- posicionaNavios tab_j 5
    threadDelay 1000000
    clearScreen
    tab_j2 <- posicionaNavios tab_j1 4
    threadDelay 1000000
    clearScreen
    tab_j3 <- posicionaNavios tab_j2 3
    threadDelay 1000000
    clearScreen
    tab_j4 <- posicionaNavios tab_j3 3
    threadDelay 1000000
    clearScreen
    tab_jf <- posicionaNavios tab_j4 2
    threadDelay 1000000
    clearScreen

    printaTabEMensagem tab_jf "     Tabuleiro final:"
    threadDelay 3000000

    loopPartida tab_jf tab_jogador_ve_bot tab_bot tab_bot_ve_jogador 1

    putStrLn ""

lerGravacao :: Handle -> IO ([[String]], [[String]], [[String]], [[String]], Int)
lerGravacao fileHandler = do
    line <- hGetLine fileHandler
    return (read line :: ([[String]], [[String]], [[String]], [[String]], Int))

carregarGravacao :: IO([[String]], [[String]], [[String]], [[String]], Int)
carregarGravacao = do
    putStrLn "Carregando jogo..."
    file <- openFile "save.txt" ReadMode
    lerGravacao file

carregarJogo :: IO ()
carregarJogo = do
    (tab_jf, tab_jogador_ve_bot, tab_bot, tab_bot_ve_jogador, round) <- carregarGravacao
    loopPartida tab_jf tab_jogador_ve_bot tab_bot tab_bot_ve_jogador round

loopPartida :: [[String]] -> [[String]] -> [[String]] -> [[String]] -> Int -> IO ()
loopPartida tab_jogador tab_jogador_ve_bot tab_bot tab_bot_ve_jogador round =
    do
        -- Conta o numero de navios restantes, caso o número de algum player chegue a 0, o jogo acaba.
        -- Caso o jogador chegue a 0, o bot ganha.
        -- Caso o bot chegue a 0, o jogador ganha.
        let numNaviosJog = contaNavios tab_jogador
        let numNaviosBot = contaNavios tab_bot
        clearScreen 
        putStrLn ("Numero de navios restantes do jogador: " ++ show numNaviosJog)
        putStrLn ("Numero de navios restantes do bot: " ++ show numNaviosBot)
        putStrLn ""

        if numNaviosJog == 0 then
            derrota
        else if numNaviosBot == 0 then
            vitoria
        else
            do
                printaTabEMensagem tab_jogador "   Tabuleiro do jogador"
                printaTabEMensagem tab_jogador_ve_bot "   Tabuleiro de ataque"
                putStrLn ("Round: " ++ show round)
                putStrLn ""

                putStrLn "Gostaria de (d)isparar ou (s)alvar? (Isso ira sobrescrever a gravacao existente)"

                opcao <- getLine

                case opcao of {
                    "d" -> putStrLn "Iniciando round.";
                    "s" -> salvarJogo tab_jogador tab_jogador_ve_bot tab_bot tab_bot_ve_jogador round;
                    _ -> do
                        putStrLn "Opcao invalida!"
                        loopPartida tab_jogador tab_jogador_ve_bot tab_bot tab_bot_ve_jogador round
                }

                (tab_bot_final, tab_jogador_ve_bot_final) <- disparaAoBot tab_bot tab_jogador_ve_bot
                putStrLn "Vez do bot..."
                threadDelay 300000
                (tab_jogador_final, tab_bot_ve_jogador_final) <- disparaAoPlayer tab_jogador tab_bot_ve_jogador
                
                clearScreen
                loopPartida tab_jogador_final tab_jogador_ve_bot_final tab_bot_final tab_bot_ve_jogador_final (round + 1)

-- TODO: Implementar a funcao de salvar jogo
salvarJogo :: [[String]] -> [[String]] -> [[String]] -> [[String]] -> Int -> IO ()
salvarJogo tab_jogador tab_jogador_ve_bot tab_bot tab_bot_ve_jogador round = do
    putStrLn "Salvando jogo..."
    writeFile "save.txt" (show (tab_jogador, tab_jogador_ve_bot, tab_bot, tab_bot_ve_jogador, round))
    putStrLn "Jogo salvo!"
    loopPartida tab_jogador tab_jogador_ve_bot tab_bot tab_bot_ve_jogador round

disparaAoBot :: [[String]] -> [[String]] -> IO ([[String]], [[String]])
disparaAoBot tab_bot tab_jogador_ve_bot = do
    putStrLn "Sua vez de disparar, escolha as posicoes X (de 1 a 10) e Y (de 1 a 10)."
    putStr "X: "
    raw_x <- getLine
    putStr "Y: "
    raw_y <- getLine

    let x = read raw_x :: Int
    let y = read raw_y :: Int

    if x < 1 || x > 10 then
        do
            putStrLn "O valor X eh invalido, insira um valor entre 1 e 10."
            threadDelay 2500000
            disparaAoBot tab_bot tab_jogador_ve_bot

    else if y < 1 || y > 10 then
        do
            putStrLn "O valor Y eh invalido, insira um valor entre 1 e 10."
            threadDelay 2500000
            disparaAoBot tab_bot tab_jogador_ve_bot
    else if (tab_jogador_ve_bot !! (x - 1)) !! (y - 1) `elem` ["X", "o"] then
        do
            putStrLn "Voce ja disparou nesta posicao. Escolha uma outra posicao."
            disparaAoBot tab_bot tab_jogador_ve_bot
    else
        if (tab_bot !! (x - 1)) !! (y - 1) == "#" then
            do
                putStrLn "Voce acertou um navio!"
                let simbolo = "X"
                let tab_bot_final = disparaEmNavio tab_bot x y simbolo
                let tab_jogador_ve_bot_final = disparaEmNavio tab_jogador_ve_bot x y simbolo
                return (tab_bot_final, tab_jogador_ve_bot_final)
        else
            do
                putStrLn "Voce acertou na agua!"
                let simbolo = "o"
                let tab_jogador_ve_bot_final = disparaEmNavio tab_jogador_ve_bot x y simbolo
                -- printaTabEMensagem tab_bot "Tab bot"
                -- threadDelay 2500000
                -- printaTabEMensagem tab_jogador_ve_bot_final "Tab jogador"
                -- threadDelay 2500000
                return (tab_bot, tab_jogador_ve_bot_final)

disparaAoPlayer :: [[String]] -> [[String]] -> IO ([[String]], [[String]])
disparaAoPlayer tab_jogador tab_bot_ve_jogador = do
    x <- randomPos
    y <- randomPos

    -- Posição inválida ou posição já foi atingida
    if x < 1 || x > 10 || y < 1 || y > 10 || (tab_bot_ve_jogador !! (x - 1)) !! (y - 1) `elem` ["X", "o"] then
        do
            disparaAoPlayer tab_jogador tab_bot_ve_jogador
    else
        -- Atingiu um navio
        if (tab_jogador !! (x - 1)) !! (y - 1) == "#" then
            do
                let simbolo = "X"
                let tab_jogador_final = disparaEmNavio tab_jogador x y simbolo
                let tab_bot_ve_jogador_final = disparaEmNavio tab_bot_ve_jogador x y simbolo
                return (tab_jogador_final, tab_bot_ve_jogador_final)
        -- Atingiu água
        else
            do
                let simbolo = "o"
                let tab_jogador_ve_bot_final = disparaEmNavio tab_bot_ve_jogador x y simbolo
                return (tab_jogador, tab_jogador_ve_bot_final)

disparaEmNavio :: [[String]] -> Int -> Int -> String -> [[String]]
disparaEmNavio (h:t) x y simbolo
    | x_index == 0 && t == [] = [[h !! k | k <- [0..(y_index - 1)]] ++ [simbolo] ++ [h !! k | k <- [(y_index + 1)..9]]]
    | x_index == 0 = ([h !! k | k <- [0..(y_index - 1)]] ++ [simbolo] ++ [h !! k | k <- [(y_index + 1)..9]]) :
        disparaEmNavio t (x - 1) y simbolo
    | null t = [h]
    | otherwise = h : disparaEmNavio t (x - 1) y simbolo
    where
        x_index = x - 1
        y_index = y - 1

vitoria :: IO ()
vitoria = do
    putStrLn "Voce venceu!"
    putStrLn "Gostaria de jogar novamente? (s ou n)"
    input <- getLine

    case input of {
        "s" -> main;
        "n" -> die "Fechando jogo...";
        _ -> do
            putStrLn "Opcao invalida!"
            vitoria
    }

derrota :: IO ()
derrota = do
    putStrLn "Voce perdeu!"
    putStrLn "Gostaria de jogar novamente? (s ou n)"
    input <- getLine

    case input of {
        "s" -> main;
        "n" -> die "Fechando jogo...";
        _ -> do
            putStrLn "Opcao invalida!"
            derrota
    }

contaNavios :: [[String]] -> Int
contaNavios tab_jogador = do
    let boolsNavioPresente = [verificaTemNavioHorizontal tab_jogador x y 1 | x <- [1..10], y <- [1..10]]
    sum ([1 | bool <- boolsNavioPresente, not bool])

montaTabuleiro :: String -> IO [[String]]
montaTabuleiro opcao =
    case opcao of {
        "" -> return tab;
        "B" -> montaTabuleiroBotInteiro tab
    }
    where
        tab = [["~" | _ <- [1..10]] | _ <- [1..10]]

montaTabuleiroBotInteiro :: [[String]] -> IO [[String]]
montaTabuleiroBotInteiro tab = do
    tab_b1 <- montaTabuleiroBot tab 5
    tab_b2 <- montaTabuleiroBot tab_b1 4
    tab_b3 <- montaTabuleiroBot tab_b2 3
    tab_b4 <- montaTabuleiroBot tab_b3 3
    montaTabuleiroBot tab_b4 2    

randomPos :: IO Int
randomPos = randomRIO (fromInteger(1),fromInteger(10))

randomBool :: IO Int
randomBool = randomRIO (fromInteger(0),fromInteger(1))

montaTabuleiroBot :: [[String]] -> Int -> IO [[String]]
montaTabuleiroBot tab tamNavio = do
    x <- randomPos
    y <- randomPos
    orient_i <- randomBool

    if orient_i == 0 then
        if (y + tamNavio - 1 <= 10) && verificaTemNavioHorizontal tab x y tamNavio then
            return (posicionaNaviosHorizontal tab x y tamNavio) else
            montaTabuleiroBot tab tamNavio
    else
        if (x + tamNavio - 1 <= 10) && verificaTemNavioVertical tab x y tamNavio then
            return (transpose (posicionaNaviosHorizontal (transpose tab) y x tamNavio)) else
            montaTabuleiroBot tab tamNavio

printaTabEMensagem :: [[String]] -> String -> IO ()
printaTabEMensagem tab msg = do
    putStrLn msg
    putStrLn (preparaTabParaPrint tab 0)

preparaTabParaPrint :: [[String]] -> Int -> String
preparaTabParaPrint [] i = ""
preparaTabParaPrint [[]] i = ""
preparaTabParaPrint tab 0 = "   1 2 3 4 5 6 7 8 9 10\n" ++ preparaTabParaPrint tab 1
preparaTabParaPrint (h:t) 10 = show 10 ++ " " ++ intersperse ' ' (concat h) ++ "\n"
preparaTabParaPrint (h:t) i = " " ++ show i ++ " " ++ intersperse ' ' (concat h) ++ "\n" ++ preparaTabParaPrint t (i + 1)

posicionaNavios :: [[String]] -> Int -> IO[[String]]
posicionaNavios tab tamNavio = do

    printaTabEMensagem tab "      Seu tabuleiro"
    putStrLn "Insira as posicoes X (de 1 a 10) Y (de 1 a 10) ORIENTACAO (H ou V) para posicionar seu navio."
    putStrLn ("Tamanho do navio: " ++ show tamNavio)

    putStr "X: "
    raw_x <- getLine
    putStr "Y: "
    raw_y <- getLine
    putStr "ORIENTACAO: "
    orient <- getLine

    let x = read raw_x :: Int
    let y = read raw_y :: Int

    if x < 1 || x > 10 then
        do
            putStrLn "O valor X eh invalido, insira um valor entre 1 e 10."
            threadDelay 2500000
            posicionaNavios tab tamNavio

    else if y < 1 || y > 10 then
        do
            putStrLn "O valor Y eh invalido, insira um valor entre 1 e 10."
            threadDelay 2500000
            posicionaNavios tab tamNavio

    else if (orient /= "H") && (orient /= "V") then
        do
            putStrLn "O valor de ORIENT eh invalido, insira o valor H para Horizontal ou V para Vertical."
            threadDelay 4000000
            posicionaNavios tab tamNavio
    else if orient == "H" then
        if y + tamNavio - 1 <= 10 then
            if verificaTemNavioHorizontal tab x y tamNavio then
                return (posicionaNaviosHorizontal tab x y tamNavio)
            else
            do
                putStrLn "Ja ha um navio nesta posicao, insira novamente."
                threadDelay 3000000
                posicionaNavios tab tamNavio
        else
            do
                putStrLn "O x + tamanho do navio estao fora dos limites, escolha outra posicao"
                threadDelay 3000000
                posicionaNavios tab tamNavio

    else if orient == "V" then
        if x + tamNavio - 1 <= 10 then
            if verificaTemNavioVertical tab x y tamNavio then
                return (transpose (posicionaNaviosHorizontal (transpose tab) y x tamNavio))
            else
                do
                    putStrLn "Ja ha um navio nesta posicao, insira novamente."
                    threadDelay 3000000
                    posicionaNavios tab tamNavio
        else
            do
                putStrLn "O y + tamanho do navio estao fora dos limites, escolha outra posicao"
                threadDelay 3000000
                posicionaNavios tab tamNavio

    else return []

verificaTemNavioHorizontal :: [[String]] -> Int -> Int -> Int -> Bool
verificaTemNavioHorizontal tab x y tamNavio =
    not (temNavio(take tamNavio (drop (y - 1) (tab !! (x - 1)))))

verificaTemNavioVertical :: [[String]] -> Int -> Int -> Int -> Bool
verificaTemNavioVertical tab x y tamNavio =
    not (temNavio(take tamNavio (drop (x - 1) ((transpose (tab)) !! (y - 1)))))

posicionaNaviosHorizontal :: [[String]] -> Int -> Int -> Int -> [[String]]
posicionaNaviosHorizontal tab_j x y tamNavio
    | not (temNavio(take tamNavio (drop (y - 1) linha))) =
        remontaNaviosHorizontal tab_j tamNavio linhaInserir posInserir
    | otherwise = []
    where
        linha = tab_j !! (x - 1)
        linhaInserir = x - 1
        posInserir = y - 1

remontaNaviosHorizontal :: [[String]] -> Int -> Int -> Int -> [[String]]
remontaNaviosHorizontal (h:t) tamNavio linhaInserir posInserir
    | linhaInserir == 0 && t == [] = [[h !! x | x <- [0..(posInserir - 1)]] ++ ["#" | _  <- [posInserir..(posInserir + tamNavio - 1)]] ++ [h !! x | x <- [(posInserir + tamNavio)..9]]]
    | linhaInserir == 0 = ([h !! x | x <- [0..(posInserir - 1)]] ++ ["#" | _  <- [posInserir..(posInserir + tamNavio - 1)]] ++ [h !! x | x <- [(posInserir + tamNavio)..9]]) :
        remontaNaviosHorizontal t tamNavio (linhaInserir - 1) posInserir
    | null t = [h]
    | otherwise = h : remontaNaviosHorizontal t tamNavio (linhaInserir - 1)  posInserir

temNavio :: [String] -> Bool
temNavio l =
    True `elem` map (== "#") l
