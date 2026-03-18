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
fnc (And f1 (Or f2 f3)) = fnn(Or (And f1 f2) (And f1 f3))
fnc (Or f1 (And f2 f3)) = fnn(And (Or f1 f2) (Or f1 f3))
fnc (And (Or f1 f2) (f3)) = fnn(Or (And f1 f3) (And f2 f3))
fnc (Or (And f1 f2) f3) = fnn(And (Or f1 f3) (Or f2 f3))
fnc x = fnn(x)

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas = undefined

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