{-- Batalha Naval --}
import System.Random -- cabal install random
import Control.Concurrent
import System.Console.ANSI -- cabal install ansi-terminal
import Data.List
-- import Control.Monad.IO.Class (MonadIO (..))
-- import Control.Monad.IO.Class -- cabal install mtl

main :: IO()
main = do
    clearScreen
    putStrLn "Vai comecar o jogo!"
    putStrLn "O que voce ira fazer?"
    putStrLn "(n) - Novo Jogo"
    putStrLn "(c) - Carregar Jogo"
    putStrLn "(s) - Sair do Jogo"

    input <- getLine

    case input of {
        "n" -> novoJogo;
        -- 'c' -> carregarJogo;
        -- 's' -> sairJogo;
        _ -> do
            putStrLn "Opcao invalida!"
            main
    }
    putStrLn ""

novoJogo :: IO()
novoJogo = do
    tab_j <- montaTabuleiro ""
    print tab_j
    tab_bot <- montaTabuleiro "B"
    print tab_bot
    tab_jogador_ve_bot <- montaTabuleiro ""
    tab_bot_ve_jogador <- montaTabuleiro ""

    print tab_bot

    threadDelay 2000000

    printaTabEMensagem tab_bot "Este eh o tabuleiro do bot, nao fala pra ninguem que voce viu..."

    threadDelay 5000000

    printaTabEMensagem tab_j "Hora da preparacao, escolha as posicoes de seus navios!"

    threadDelay 1000000
    tab_j1 <- posicionaNavios tab_j 5
    threadDelay 1000000
    tab_j2 <- posicionaNavios tab_j1 4
    threadDelay 1000000
    tab_j3 <- posicionaNavios tab_j2 3
    threadDelay 1000000
    tab_j4 <- posicionaNavios tab_j3 3
    threadDelay 1000000
    tab_jf <- posicionaNavios tab_j4 2
    threadDelay 1000000

    printaTabEMensagem tab_jf ""

    putStrLn ""

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
    clearScreen
    putStrLn "Tabuleiro do Jogador"
    putStrLn (preparaTabParaPrint tab)
    putStrLn msg

preparaTabParaPrint :: [[String]] -> String
preparaTabParaPrint [] = ""
preparaTabParaPrint [[]] = ""
preparaTabParaPrint (h:t) = intersperse ' ' (concat h) ++ "\n" ++ preparaTabParaPrint t

posicionaNavios :: [[String]] -> Int -> IO[[String]]
posicionaNavios tab tamNavio = do

    printaTabEMensagem tab "Insira as posicoes X (de 1 a 10) Y (de 1 a 10) ORIENTACAO (H ou V) para posicionar seu navio:"
    putStrLn ("Inserir navio de tamanho " ++ show tamNavio)

    putStr "X: "
    raw_x <- getLine
    putStr "Y: "
    raw_y <- getLine
    putStr "ORIENT: "
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
    False `elem` map (== "~") l
