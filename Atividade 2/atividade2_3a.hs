{- ATIVIDADES 2 -}
{- Renan Soares Germano -}
{- Bruno Albuquerque Brito -}
{- Michele Mathias -}

import Control.Monad

main :: IO ()
main = 
    putStrLn "Entre com um numero: " >>
    (readLn :: IO Int) >>= \ x ->
    putStrLn $ (show x) ++ " é " ++ (boolToParImpar $ even x)
        where
            boolToParImpar True = "Par"
            boolToParImpar False = "Impar"