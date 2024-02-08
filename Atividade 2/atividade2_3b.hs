{- ATIVIDADES 2 -}
{- Renan Soares Germano -}
{- Bruno Albuquerque Brito -}
{- Michele Mathias -}

import Control.Monad

main :: IO ()
main = 
    putStrLn "Entre com um texto: " >>
    (getLine :: IO String) >>= \ str ->
    putStrLn $ reverse str