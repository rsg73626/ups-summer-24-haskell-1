
import System.IO

main :: IO ()
main = do
    putStrLn "Digite um número inteiro n:"
    input <- getLine
    let n = read input :: Int
    if n > 0
        then do
            putStrLn $ "Digite " ++ show n ++ " linhas:"
            contents <- sequence (replicate n getLine)
            writeFile "linhas.txt" (unlines contents)
            putStrLn "Linhas gravadas no arquivo 'linhas.txt'."
        else
            putStrLn "Número inválido. O número deve ser maior que zero."