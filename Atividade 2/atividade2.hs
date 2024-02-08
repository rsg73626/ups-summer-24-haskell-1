{- ATIVIDADES 2 -}
{- Renan Soares Germano -}
{- Bruno Albuquerque Brito -}
{- Michele Mathias -}

{- 1 - Funtores -}

data Coisa a = UmaCoisa a | DuasCoisas a a | ZeroCoisa deriving Show

{- (a) Faca uma instancia de Functor para o tipo Coisa. A funcao g deve ”ir para dentro” em todas as coordenadas de Coisa. No caso de ZeroCoisa , o fmap deve retornar ZeroCoisa . -}

instance Functor Coisa where
    fmap _ ZeroCoisa = ZeroCoisa
    fmap g (UmaCoisa x) = UmaCoisa (g x)
    fmap g (DuasCoisas x y) = DuasCoisas (g x) (g y)

zeroCoisa::(Coisa Int)
coisas::[(Coisa Int)]
zeroCoisa  = ZeroCoisa
umaCoisa   = UmaCoisa 10
duasCoisas = DuasCoisas 20 30
coisas     = [zeroCoisa, umaCoisa, duasCoisas]
coisasDuplicadas = map (\elemento -> fmap (2*) elemento) coisas
coisasTriplicadas = map (\elemento -> (3*) <$> elemento) coisas

{- (b) Aproveitando o exercıcio anterior, faca uma instancia de Applicative Functor para o tipo Coisa . -}

instance Applicative Coisa where
    pure x = UmaCoisa x
    (<*>) (UmaCoisa f) (UmaCoisa x)         = UmaCoisa (f x)
    (<*>) (UmaCoisa f) (DuasCoisas x y)     = DuasCoisas (f x) (f y)
    (<*>) (DuasCoisas f g) (UmaCoisa x)     = DuasCoisas (f x) (g x)
    (<*>) (DuasCoisas f g) (DuasCoisas x y) = DuasCoisas (f x) (g y)
    (<*>) _ _                               = ZeroCoisa

{- (c) Crie a funcao " mult234 :: Double -> Coisa Double " que multiplica por 2 a primeira coordenada, por 3 a segunda, e por 4 a terceira. Use a instancia de Applicative feita no exercıcio anterior. -}

mult23 :: Double -> Coisa Double
mult23 x = (DuasCoisas (2*) (3*)) <*> (DuasCoisas x x)

{- (d) Escreva uma instˆancia para Functor e Applicative para o tipo Arvore da lista de exerc ́ıcio anterior. -}

data Arvore a = Galho a (Arvore a) (Arvore a) | Folha a | Vazio deriving Show

arvore::(Arvore Int) = Galho 15 (Galho 11 (Folha 6) (Galho 12 (Folha 10) Vazio)) (Galho 20 Vazio (Galho 22 (Folha 21) Vazio))

instance Functor Arvore where
    fmap _ Vazio                = Vazio
    fmap f (Folha x)            = Folha (f x)
    fmap f (Galho x left right) = Galho (f x) (fmap f left) (fmap f right)

instance Applicative Arvore where
    pure x = Folha x
    (<*>) (Folha f) (Folha x)                = Folha (f x)
    (<*>) (Folha f) (Galho x left right)     = Galho (f x) (fmap f left) (fmap f right)
    (<*>) (Galho f left right) (Folha x)     = Galho (f x) (left <*> (Folha x)) (right <*> (Folha x))
    (<*>) (Galho f left right) (Galho x y z) = Galho (f x) (left <*> y) (right <*> z)
    (<*>) _ _                                = Vazio

{- 2. Sobre mˆonadas. -}

{- 2.1. Faca um tipo Caixa com um type parameter a e tres construtores chamados Um , Dois e Tres possuindo um, dois e tres campos de tipo a , respectivamente. -}

data Caixa a = Um a | Dois a a | Tres a a a deriving Show

{- Faca uma instancia de Functor para o tipo Caixa. A funcao deve ser aplicada em todas as coordenadas dos valores ( Um , Dois ou Tres ). -}

instance Functor Caixa where
    fmap f (Um x) = Um (f x)
    fmap f (Dois x y) = Dois (f x) (f y)
    fmap f (Tres x y z) = Tres (f x) (f y) (f z)

caixasDeString = [(Um "Renan"), (Dois "Renan" "Soares"), (Tres "Renan" "Soares" "Germano")]
caixasDeTamanhos = map (\x -> fmap length x) caixasDeString

{- Crie uma instancia de Monad para o tipo Caixa . Seu return deve ser o value constructor de Um. -}

instance Applicative Caixa where
    pure x = Um x
    (<*>) (Um f) (Um x)             = Um (f x)
    (<*>) (Um f) (Dois x _)         = Um (f x)
    (<*>) (Um f) (Tres x _ _)       = Um (f x)
    (<*>) (Dois f _) (Um x)         = Um (f x)
    (<*>) (Dois f g) (Dois x y)     = Dois (f x) (g y)
    (<*>) (Dois f g) (Tres x y _)   = Dois (f x) (g y)
    (<*>) (Tres f _ _) (Um x)       = Um (f x)
    (<*>) (Tres f g _) (Dois x y)   = Dois (f x) (g y)
    (<*>) (Tres f g h) (Tres x y z) = Tres (f x) (g y) (h z)

instance Monad Caixa where
    return = pure
    (Um x) >>= f       = f x
    (Dois _ y) >>= f   = f y
    (Tres _ _ z) >>= f = f z

{- 2.2. Crie uma fun ̧c ̃ao mult234 :: Double -> Caixa Double que receba uma parˆametro x e devolva o dobro de x na primeira coordenada, o triplo na segunda e o qu ́adruplo na terceira usando o operador >>= -}

mult234 :: Double -> Caixa Double
mult234 x = return (2 * x)
    >>= (\y -> return $ Dois y (3 * x))
    >>= (\(Dois y z) -> Tres y z (4 * x))

{- 3. Monada I.O. -}

{- (a) Faca um programa que faca o usu ́ario digitar um numero e mostre na saıda padrao se ele e par ou ımpar. -}

{- RESPOSTA: arquivo atividade2_3a.hs -}

{- (b) Fac ̧a um programa que mostre uma palavra em ordem reversa a partir de uma digitada pelo usu ́ario. -}

{- RESPOSTA: arquivo atividade2_3b.hs -}

{- (c) Fac ̧a um programa que calcule uma equa ̧c ̃ao do segundo grau, a partir dos dados digitados pelo usu ́ario. -}

{- RESPOSTA: arquivo atividade2_3c.hs -}

{- (d) -}

{- RESPOSTA: arquivo atividade2_3d.hs -}