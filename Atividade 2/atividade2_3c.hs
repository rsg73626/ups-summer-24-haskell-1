
import Control.Monad

main :: IO ()
main = do
    putStrLn "Entre com os termos da equação do segundo grau (a x² + b x + c): "
    putStrLn "a: "
    a <- (readLn :: IO Int)
    putStrLn "b: "
    b <- (readLn :: IO Int)
    putStrLn "c: "
    c <- (readLn :: IO Int)
    putStrLn $ generateOutput (Equation (fromIntegral a) (fromIntegral b) (fromIntegral c))

data Equation = Equation{ a :: Float, b :: Float, c :: Float }
data EquationResult = Solution { x1 :: Float, x2 :: Float } | NoSolution { delta :: Float } 

generateOutput :: Equation -> String
generateOutput eq = (equationToString eq) ++ "\n" ++ (equationResultToString eqResult) ++ "\n" ++ (generateRealProof eq eqResult)
    where
        eqResult = solveEquation eq

solveEquation :: Equation -> EquationResult
solveEquation (Equation a b c)
    | delta < 0 = NoSolution delta
    | otherwise = Solution x1 x2
    where
        delta = (b * b) + ((-4) * a * c)
        x1 = ((-b) + (sqrt delta)) / (2 * a)
        x2 = ((-b) - (sqrt delta)) / (2 * a)

equationToString :: Equation -> String
equationToString (Equation a b c) = (equationTermToString a True) ++ "x² " ++ (equationTermToString b False) ++ "x " ++ (equationTermToString c False)

equationTermToString :: Float -> Bool -> String
equationTermToString x isFirstElement
    | (x < 0 && isFirstElement) || isFirstElement = show $ round x
    | x < 0 = "- " ++ (show $ round $ abs x)
    | otherwise = "+ " ++ (show $ round x)

equationResultToString :: EquationResult -> String
equationResultToString (Solution x1 x2) = "x1 = " ++ (show x1) ++ "; x2 = " ++ (show x2)
equationResultToString (NoSolution delta) = "Delta menor que zero (" ++ (show delta) ++ "). Nao ha solucao no conjunto dos reais."

generateRealProof :: Equation -> EquationResult -> String
generateRealProof _ (NoSolution _) = "Impossível calcular prova real."
generateRealProof equation (Solution x1 x2) = "x1 = " ++ (show $ calculateRealProof equation x1) ++ "\nx2 = " ++ (show $ calculateRealProof equation x2)

calculateRealProof :: Equation -> Float -> Float
calculateRealProof (Equation a b c) x = (a * (x ^ 2)) + (b * x) + c

