{-- Batalha Naval --}
import Control.Concurrent
import System.Console.ANSI -- cabal install ansi-terminal
import Data.List

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
    }
    putStrLn ""

novoJogo :: IO()
novoJogo = do
    let tab_j = montaTabuleiro ""
    let tab_bot = montaTabuleiro ""
    let tab_jogador_ve_bot = montaTabuleiro ""
    let tab_bot_ve_jogador = montaTabuleiro ""

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

montaTabuleiro :: String -> [[String]]
montaTabuleiro "" = [["~" | _ <- [1..10]] | _ <- [1..10]]

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
        if not (temNavio(take tamNavio (drop (y - 1) (tab !! (x - 1))))) then
            return (posicionaNaviosHorizontal tab x y tamNavio)
        else
            do
                putStrLn "Ja ha um navio nesta posicao, insira novamente."
                -- print (not (temNavio(take tamNavio (drop (y - 1) tab !! (x - 1)))))
                threadDelay 3000000
                posicionaNavios tab tamNavio
    else if orient == "V" then
        if not (temNavio(take tamNavio (drop (x - 1) ((transpose (tab)) !! (y - 1))))) then
            return (transpose (posicionaNaviosHorizontal (transpose tab) y x tamNavio))
        else
            do
                putStrLn "Ja ha um navio nesta posicao, insira novamente."
                threadDelay 3000000
                posicionaNavios tab tamNavio
                

    else return []

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
