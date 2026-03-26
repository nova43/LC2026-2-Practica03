module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn (Cons x) = Cons x
fnn (Var p) = Var p
fnn(Not (Not f1)) = f1
fnn (Not (And f1 f2)) = fnn(Or (fnn(Not f1)) (fnn(Not f2)))
fnn (Not (Or f1 f2)) = fnn(And (fnn(Not f1)) (fnn(Not f2)))
fnn (Impl f1 f2) = Or (fnn(Not f1)) (fnn(f2))
fnn (Syss f1 f2) = And (fnn(Impl f1 f2)) (fnn(Impl f2 f1))
fnn (And f1 f2) = And (fnn f1) (fnn f2)
fnn (Or f1 f2) = Or (fnn f1) (fnn f2)
fnn (Not f1) = Not (fnn f1)

--Ejercicio 2
fnc :: Prop -> Prop
fnc prop = fncAux(fnn prop)

fncAux :: Prop -> Prop
fncAux (And a b) = And (fncAux a) (fncAux b)
fncAux (Or a b) = distribuir (fncAux a) (fncAux b)
fncAux x = x

distribuir :: Prop -> Prop -> Prop
distribuir f1 (And f2 f3) = And (distribuir f1 f2) (distribuir f1 f3)
distribuir (And f1 f2) f3 = And (distribuir f1 f3) (distribuir f2 f3)
distribuir f1 f2 = Or f1 f2

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas (Cons x) = [[]]
clausulas (Var p) = [[Var p]]
clausulas (Not p) = [[Not p]]
clausulas (Or p q) = [clausulasAux(Or p q)]
clausulas (And p q) = (clausulas(p) ++ clausulas(q))

clausulasAux :: Prop -> Clausula
clausulasAux (Cons _) = []
clausulasAux (Var p) = [Var p]
clausulasAux (Not p) = [(Not p)]
clausulasAux (Or f1 f2) = quitarRepetidos(clausulasAux(f1) ++ clausulasAux(f2))
clausulasAux x = []

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

quitarRepetidos :: Eq a => [a] -> [a]
quitarRepetidos [] = []
quitarRepetidos (x:xs)
    | pertenece x xs = quitarRepetidos xs
    | otherwise = x : quitarRepetidos xs

--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion = undefined

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente = undefined

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion = undefined