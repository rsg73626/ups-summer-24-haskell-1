
{- ATIVIDADES -}
{- Renan Soares Germano -}
{- Bruno Albuquerque Brito -}
{- Michele Mathias -}

{- 1. -}

{- 1.1 -}

{- (a) -}

multipleOf50 = [ x | x <- [0..50], (mod x 5) == 0 ]

{- (b) -}

consonants = [ x | x <- ['a'..'z'], x /= 'a', x /= 'e', x /= 'i', x /= 'o', x /= 'u']

{- (c) -}

numbers = [ x | x <- [0..50], x /= 2, x /= 7, x /= 13, x /= 35, x /= 42]

{- (d) -}

chessTableCoordenates = [ (x, y) | x <- ['a'..'h'], y <- [1..8]]

{- 1.2 -}

{- (a) -}

isEvenLengthString :: String -> Bool
isEvenLengthString string = (mod (length string) 2) == 0

{- (b) -}

reverseStrings :: [String] -> [String]
reverseStrings []     = []
reverseStrings (x:xs) = (reverseStrings xs) ++ [x]

{- (c) -}

meuHead :: [Int] -> Maybe Int
meuHead [] = Nothing
meuHead lista = Just (sum $ take 1 lista)
-- meuHead lista = Just (foldl (+) 0 (take 1 lista)) -- this is also possible

{- (d) -}

intToBin :: Int -> [Int]
intToBin 0 = [0]
intToBin 1 = [1]
intToBin x = (intToBin (div x 2)) ++ [(mod x 2)]

{- 2 -}

{- 2.1 -}

data Mes = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez deriving Show
data Hemisferio = Norte | Sul deriving Show
data EstacaoDoAno = Verao | Outono | Primavera | Inverno deriving Show
meses = [ Jan, Fev, Mar, Abr, Mai, Jun, Jul, Ago, Set, Out, Nov, Dez ]

checaFim :: Mes -> Int
checaFim Jan = 31
checaFim Fev = 28
checaFim Mar = 31
checaFim Abr = 30
checaFim Mai = 31
checaFim Jun = 30
checaFim Jul = 31
checaFim Ago = 30
checaFim Set = 31
checaFim Out = 30
checaFim Nov = 31
checaFim Dez = 30

prox :: Mes -> Mes
prox Jan = Fev
prox Fev = Mar
prox Mar = Abr
prox Abr = Mai
prox Mai = Jun
prox Jun = Jul
prox Jul = Ago
prox Ago = Set
prox Set = Out
prox Out = Nov
prox Nov = Dez
prox Dez = Jan

estacaoDoAno :: Hemisferio -> Mes -> EstacaoDoAno
estacaoDoAno Norte  Jan = Inverno
estacaoDoAno Norte  Fev = Inverno
estacaoDoAno Norte  Mar = Primavera
estacaoDoAno Norte  Abr = Primavera
estacaoDoAno Norte  Mai = Primavera
estacaoDoAno Norte  Jun = Verao
estacaoDoAno Norte  Jul = Verao
estacaoDoAno Norte  Ago = Verao
estacaoDoAno Norte  Set = Outono
estacaoDoAno Norte  Out = Outono
estacaoDoAno Norte  Nov = Outono
estacaoDoAno Norte  Dez = Inverno
estacaoDoAno Sul  Jan = Verao
estacaoDoAno Sul  Fev = Verao
estacaoDoAno Sul  Mar = Outono
estacaoDoAno Sul  Abr = Outono
estacaoDoAno Sul  Mai = Outono
estacaoDoAno Sul  Jun = Inverno
estacaoDoAno Sul  Jul = Inverno
estacaoDoAno Sul  Ago = Inverno
estacaoDoAno Sul  Set = Primavera
estacaoDoAno Sul  Out = Primavera
estacaoDoAno Sul  Nov = Primavera
estacaoDoAno Sul  Dez = Verao

estacoesDoAno = [ (x, y, (estacaoDoAno x y)) | x <- [Norte, Sul], y <- meses ]

{- 2.2 -}

data Cripto = Mensagem String | Cifrado String | Erro deriving (Show, Eq)

encriptar :: Cripto -> Cripto
encriptar (Cifrado _) = Erro
encriptar (Mensagem texto) = (Cifrado [ succ x | x <- texto ])

decriptar :: Cripto -> Cripto
decriptar (Mensagem _) = Erro
decriptar (Cifrado codigo) = (Mensagem [ pred x | x <- codigo ])

textos = ["Renan", "Germano", "aluno", "do", "curso", "de", "verao", "USP", "2024"]
cifrados = [encriptar (Mensagem x) | x <- textos]
mensagens = [decriptar x | x <- cifrados]
testesCripSucesso = [(Mensagem x) | x <- textos] == mensagens

{- 3 -}

{- 3.1 -}

filterEvenNumbers :: [Int] -> [Int]
filterEvenNumbers [] = []
filterEvenNumbers (x:xs)
    | (mod x 2) == 0 = [x] ++ filterEvenNumbers xs
    | otherwise      = filterEvenNumbers xs

filterOddNumbers :: [Int] -> [Int]
filterOddNumbers [] = []
filterOddNumbers (x:xs)
    | (mod x 2) == 0 = filterOddNumbers xs
    | otherwise      = [x] ++ filterOddNumbers xs

evenNumbers = filterEvenNumbers [0..100]
oddNumbers = filterOddNumbers [0..100]

{- 3.2 -}

data Correncia = Real | Dolar deriving Show
data Dinheiro = Dinheiro { valor::Float, correncia::Correncia } deriving Show

dinheiros = [ (Dinheiro 1 Real), (Dinheiro 2 Dolar), (Dinheiro 3 Real), (Dinheiro 4 Dolar), (Dinheiro 5 Real), (Dinheiro 6 Dolar), (Dinheiro 7 Real), (Dinheiro 8 Dolar), (Dinheiro 9 Real), (Dinheiro 10 Dolar) ]

converteParaReal :: [Dinheiro] -> [Dinheiro]
converteParaReal [] = []
converteParaReal ((Dinheiro valor Dolar):restoLista) = (Dinheiro valor Real) : (converteParaReal restoLista)
converteParaReal (dinheiro:restoLista) = dinheiro : (converteParaReal restoLista)

converteParaDolar :: [Dinheiro] -> [Dinheiro]
converteParaDolar [] = []
converteParaDolar ((Dinheiro valor Real):restoLista) = (Dinheiro valor Dolar) : (converteParaDolar restoLista)
converteParaDolar (dinheiro:restoLista) = dinheiro : (converteParaDolar restoLista)

{- (a) -}

filtraDolares :: [Dinheiro] -> [Dinheiro]
filtraDolares [] = []
filtraDolares ((Dinheiro _ Real):restoLista) = filtraDolares restoLista
filtraDolares (dinheiro:restoLista) = dinheiro : (filtraDolares restoLista)

{- (b) -}

somaDolares :: [Dinheiro] -> Float
somaDolares [] = 0
somaDolares ((Dinheiro valor Dolar):restoLista) = valor + (somaDolares restoLista)
somaDolares (_:restoLista) = somaDolares restoLista

{- (c) -}

somaReais :: Float -> [Dinheiro] -> [Dinheiro]
somaReais _ [] = []
somaReais incremento ((Dinheiro valor Real):restoLista) = (Dinheiro (valor + incremento) Real) : (somaReais incremento restoLista)
somaReais incremento (dinheiro:restoLista) = dinheiro : (somaReais incremento restoLista)

{- 4 -}

{- 4.1 -}

data Lista a = Nulo | a :>: (Lista a) deriving Show

listaInts = (1 :>: (2 :>: (3 :>: (4 :>: (5 :>: Nulo)))))

removerElemento :: (Eq a) => a -> Lista a -> Lista a
removerElemento _ Nulo = Nulo
removerElemento elemento (atual :>: resto)
    | atual == elemento = resto
    | otherwise         = atual :>: (removerElemento elemento resto)

{- 4.2 -}

data Paridade = Par | Impar deriving Show

class ParImpar a where
    decide :: a -> Paridade

instance ParImpar Int where 
    decide a 
        | (mod a 2) == 0 = Par
        | otherwise      = Impar

paresEImparesInts = [ (x, decide (x::Int)) | x <- [0..20] ]

instance ParImpar [a] where
    decide list = decide (length list)

paresEImparesListas = [ (x, decide x) | x <- [[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]] ]

instance ParImpar Bool where 
    decide True = Impar
    decide False = Par

paresEImparesBools = [ (x, decide x) | x <- [ True, False, True, False, True, False, True, False, True, False ] ]

{- 4.3 -}

data TipoProduto = Escritorio | Imformatica | Livro | Filme | Total deriving Show

data Produto = Produto { val::Double, tp::TipoProduto } | Nada deriving Show

instance Semigroup Produto where 
    (<>) (Produto val1 _) (Produto val2 _) = (Produto (val1 + val2) Total)
    (<>) a Nada = a
    (<>) Nada a = a

instance Monoid Produto where
    mappend = (<>)
    mempty  = Nada

p1 = Produto 10 Escritorio
p2 = Produto 20 Imformatica
p3 = Produto 30 Livro
p4 = Produto 40 Filme
produtos = [p1, p2, p3, p4]
totalProdutos = foldl (<>) Nada produtos

{- 4.4 -}

data Arvore a = Galho a (Arvore a) (Arvore a) | Folha a | Vazio deriving Show

arvore::(Arvore Int) = Galho 15 (Galho 11 (Folha 6) (Galho 12 (Folha 10) Vazio)) (Galho 20 Vazio (Galho 22 (Folha 21) Vazio))

posOrdem :: Arvore a -> [a]
posOrdem Vazio         = []
posOrdem (Folha a)     = [a]
posOrdem (Galho a b c) = (posOrdem b) ++ (posOrdem c) ++ [a]

{-
posOrdem (Galho 15 (Galho 11 (Folha 6) (Galho 12 (Folha 10) Vazio)) (Galho 20 Vazio (Galho 22 (Folha 21) Vazio))) = [6, 10, 12, 11, 21, 22, 20, 15]
    (posOrdem (Galho 11 (Folha 6) (Galho 12 (Folha 10) Vazio))) ++ (posOrdem (Galho 20 Vazio (Galho 22 (Folha 21) Vazio))) ++ [15] = [6, 10, 12, 11] ++ [21, 22, 20] ++ [15]
        posOrdem (Galho 11 (Folha 6) (Galho 12 (Folha 10) Vazio)) = [6, 10, 12, 11]
            (posOrdem (Folha 6)) ++ (posOrdem (Galho 12 (Folha 10) Vazio)) ++ [11] = [6] ++ [10, 12] ++ [11]
                posOrdem (Folha 6) = [6]
                posOrdem (Galho 12 (Folha 10) Vazio) = [10, 12]
                    (posOrdem (Folha 10)) ++ (posOrdem Vazio) ++ [12] = [10] ++ [] ++ [12]
                        posOrdem (Folha 10) = [10]
                        posOrdem Vazio      = []
        posOrdem (Galho 20 Vazio (Galho 22 (Folha 21) Vazio)) = [21, 22, 20]
            (posOrdem Vazio) ++ (posOrdem (Galho 22 (Folha 21) Vazio)) ++ [20] = [] ++ [21, 22] ++ [20]
                posOrdem Vazio = []
                posOrdem (Galho 22 (Folha 21) Vazio) = [21, 22]
                    (posOrdem (Folha 21)) ++ (posOrdem Vazio) ++ [22] = [21] ++ [] ++ 22
                        posOrdem (Folha 21) = [21]
                        posOrdem Vazio = []

-}

preOrdem :: Arvore a -> [a]
preOrdem Vazio         = []
preOrdem (Folha a)     = [a]
preOrdem (Galho a b c) = [a] ++ (preOrdem b) ++ (preOrdem c)

{-
preOrdem (Galho 15 (Galho 11 (Folha 6) (Galho 12 (Folha 10) Vazio)) (Galho 20 Vazio (Galho 22 (Folha 21) Vazio))) = [15, 11, 6, 12, 10, 20, 22, 21]
    [15] ++ (preOrdem (Galho 11 (Folha 6) (Galho 12 (Folha 10) Vazio))) ++ (preOrdem (Galho 20 Vazio (Galho 22 (Folha 21) Vazio))) = [15] ++ [11, 6, 12, 10] ++ [20, 22, 21]
        preOrdem (Galho 11 (Folha 6) (Galho 12 (Folha 10) Vazio)) =  [11, 6, 12, 10]
            [11] ++ (preOrdem (Folha 6)) ++ (preOrdem (Galho 12 (Folha 10) Vazio)) = [11] ++ [6] ++ [12, 10]
                preOrdem (Folha 6) = [6]
                preOrdem (Galho 12 (Folha 10) Vazio) = [12, 10]
                    [12] ++ (preOrdem (Folha 10)) ++ (preOrdem Vazio) = [12] ++ [10] ++ []
                        preOrdem (Folha 10) = [10]
                        preOrdem Vazio = []
        preOrdem (Galho 20 Vazio (Galho 22 (Folha 21) Vazio)) = [20, 22, 21]
            [20] ++ (preOrdem Vazio) ++ (preOrdem (Galho 22 (Folha 21) Vazio)) = [20] ++ [] ++ [22, 21]
                preOrdem Vazio = []
                preOrdem (Galho 22 (Folha 21) Vazio) = [22, 21]
                    [22] ++ (preOrdem (Folha 21)) ++ (preOrdem Vazio) = [22] ++ [21] ++ []
                        preOrdem (Folha 21) = [21]
                        preOrdem Vazio = []
-}

{- Usando a estrutura de  ́arvore vista, fa ̧ca uma fun ̧c ̃ao que some todos os elementos de uma  ́arvore de nu ́meros. -}

somaArvore :: Arvore Int -> Int
somaArvore Vazio         = 0
somaArvore (Folha a)     = a
somaArvore (Galho a b c) = a + (somaArvore b) + (somaArvore c)

{- 5 -}

data Nat = Z | Suc Nat deriving Show

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (Suc n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat n = Suc (intToNat (n - 1))

somar :: Nat -> Nat -> Nat
somar x Z = x
somar x (Suc n) = Suc (somar x n)

mult :: Nat -> Nat -> Nat
mult x Z = Z
mult x (Suc Z) = x
mult x (Suc n) = somar x (mult x n)

fatt :: Nat -> Nat
fatt Z = Suc Z
fatt (Suc n) = mult (Suc n) (fatt n)

fibb :: Nat -> Nat
fibb Z = (Suc Z)
fibb (Suc Z) = (Suc Z)
fibb (Suc (Suc n)) = somar (fibb (Suc n)) (fibb n)

tenFirstFibonaccis = [ natToInt (fibb (intToNat x)) | x <- [0..10] ]