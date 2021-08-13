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
    let tab_jogador = montaTabuleiro ""
    let tab_bot = montaTabuleiro ""
    let tab_jogador_ve_bot = montaTabuleiro ""
    let tab_bot_ve_jogador = montaTabuleiro ""

    let msgPreparacao = "Hora da preparacao, escolha as posicoes de seus navios!"

    printaTabEMensagem tab_jogador msgPreparacao

    threadDelay 1000000

    -- TODO: TRANSFORMAR ISSO EM FUNCAO
    -- TODO: TRATAR ERROS DE INPUT (i.e. X > 10, X < 0, etc)
    printaTabEMensagem tab_jogador "Insira as posicoes X Y ORIENTACAO para posicionar seu navio:\nExemplo:\nX: 2\nY: 3\nORIENT: V\nExemplo:\nX: 5\nY: 2\nORIENT: H\n"

    putStr "X: "
    raw_x <- getLine
    putStr "Y: "
    raw_y <- getLine
    putStr "ORIENT: "
    orient <- getLine

    let x = read raw_x :: Int
    let y = read raw_y :: Int

    let tab_1 = posicionaNavios tab_jogador x y orient 4

    printaTabEMensagem tab_1 ""

    threadDelay 1000000

    printaTabEMensagem tab_1 "Insira as posicoes X Y ORIENTACAO para posicionar seu navio:\nExemplo:\nX: 2\nY: 3\nORIENT: V\nExemplo:\nX: 5\nY: 2\nORIENT: H\n"

    putStr "X: "
    raw_x_2 <- getLine
    putStr "Y: "
    raw_y_2 <- getLine
    putStr "ORIENT: "
    orient_2 <- getLine

    let x_2 = read raw_x_2 :: Int
    let y_2 = read raw_y_2 :: Int

    let tab_2 = posicionaNavios tab_1 x_2 y_2 orient_2 4

    threadDelay 1000000
    
    printaTabEMensagem tab_2 ""

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

posicionaNavios :: [[String]] -> Int -> Int -> String -> Int -> [[String]]
posicionaNavios l x y orient tamNavio
  | orient == "H" = posicionaNaviosHorizontal l x y tamNavio
  | orient == "V" = transpose (posicionaNaviosHorizontal (transpose l) y x tamNavio)
  | otherwise = []

posicionaNaviosHorizontal :: [[String]] -> Int -> Int -> Int -> [[String]]
posicionaNaviosHorizontal tab_j x y tamNavio
    | y + tamNavio > 10 = []
    | not (temNavio(take tamNavio (drop (y - 1) linha))) =
        remontaNaviosHorizontal tab_j 4 linhaInserir posInserir
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
