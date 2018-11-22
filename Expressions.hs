module Expressions (Expr(..),
                    negate',
                    simplify,
                    mapVar,
                    plugIn,
                    evalExpr,
                    evalExpr',
                    derivative,
                    ddx)

where
import Data.Complex
import Data.Char

infixl 4 :+:, :-:
infixl 5 :*:, :/:
infixr 6 :^:

data Expr a= Var Char
             | Const a
             | (Expr a) :+: (Expr a)
             | (Expr a) :*: (Expr a)
             | (Expr a) :^: (Expr a)
             | (Expr a) :/: (Expr a)
             | (Expr a) :-: (Expr a)
instance (Show a) => Show (Expr a) where
  show (Var a) =  show a
  show (Const a) =  show a
  show (a :+: b)  =  "(" ++ show a ++" + "++ show b ++")"
  show (a :-: b)  =  "(" ++ show a ++" - "++ show b ++ ")"
  show (a :*: b)  = "(" ++ show a ++" * "++ show b ++ ")"
  show (a :^: b)  =  "(" ++ show a ++ " ^ " ++ show b ++ ")"
  show (a :/: b)  =  "(" ++ show a ++ " / " ++ show b ++ ")"
testE = ((Var 'z'):^: Const (3.0:+0)) :-: (Const (1.0:+0.0))
negate' :: (Num a) => Expr a -> Expr a
negate' (Var c)    = (Const (-1)) :*: (Var c)
negate' (Const a)  = Const (-a)
negate' (a :+: b)  = (negate' a) :+: (negate' b)
negate' (a :*: b)  = (negate' a) :*: b
negate' (a :^: b)  = Const (-1) :*: a :^: b
negate' (a :/: b)  = (negate' a) :/: b

simplify :: (Num a, Eq a, Floating a) => Expr a -> Expr a
simplify (Const a :+: Const b) = Const (a + b)
simplify (a       :+: Const 0) = simplify a
simplify (Const 0 :+: a      ) = simplify a

simplify (Const a :-: Const b) = Const (a - b)
simplify (a       :-: Const 0) = simplify a
simplify (Const 0 :-: a      ) = simplify a

simplify (Const a :*: Const b) = Const (a*b)
simplify (a :*: Const 1)         = simplify a
simplify (Const 1 :*: a)         = simplify a
simplify (a :*: Const 0)         = Const 0
simplify (Const 0 :*: a)         = Const 0

simplify (Const a :^: Const b)       = Const (a**b)
simplify (a :^: Const 1)             = simplify a
simplify (a :^: Const 0)             = Const 1
simplify ((c :^: Const b) :^: Const a) = c :^: (Const (a*b))

simplify (Const a :*: (Const b :*: expr)) = (Const $ a*b) :*: (simplify expr)
simplify (Const a :*: expr :*: Const b) = (Const $ a*b) :*: (simplify expr)
simplify (expr :*: Const a :*: Const b) = (Const $ a*b) :*: (simplify expr)
simplify (Const a :*: (b :+: c))        = (Const a :*: (simplify b)) :+: (Const a :*: (simplify c))

simplify (Const 0 :/: a        ) = Const 0
simplify (Const a :/: Const 0)   = error "Division by zero!"
simplify (Const a :/: Const b)   | a == b = Const 1 -- only when a == b
simplify (a       :/: Const 1)   = simplify a

simplify (a :/: b)  = (simplify a) :/: (simplify b)
simplify (a :^: b)  = (simplify a) :^: (simplify b)
simplify (a :*: b)  = (simplify a) :*: (simplify b)
simplify (a :+: b)  = (simplify a) :+: (simplify b)
simplify (a :-: b)  = (simplify a) :-: (simplify b)
simplify x          = id x

fullSimplify expr = simplify.simplify.simplify $ expr

mapVar :: (Char -> Expr a) -> Expr a -> Expr a
mapVar f (Var d)   = f d
mapVar _ (Const a) = Const a
mapVar f (a :+: b) = (mapVar f a) :+: (mapVar f b)
mapVar f (a :*: b) = (mapVar f a) :*: (mapVar f b)
mapVar f (a :^: b) = (mapVar f a) :^: (mapVar f b)
mapVar f (a :/: b) = (mapVar f a) :/: (mapVar f b)

plugIn :: Char -> a -> Expr a -> Expr a
plugIn c val = mapVar (\x -> if x == c then Const val else Var x)


evalExpr :: (Num a, Floating a) => Char -> a -> Expr a -> a
evalExpr c x = evalExpr' . plugIn c x

evalExpr' :: (Num a, Floating a) => Expr a -> a
evalExpr' (Const a) = a
evalExpr' (Var   c) = error $ "Variables ("
                              ++ [c] ++
                              ") still exist in formula. Try plugging in a value!"
evalExpr' (a :+: b) = (evalExpr' a) + (evalExpr' b)
evalExpr' (a :*: b) = (evalExpr' a) * (evalExpr' b)
evalExpr' (a :^: b) = (evalExpr' a) ** (evalExpr' b)
evalExpr' (a :/: b) = (evalExpr' a) / (evalExpr' b)

derivative :: (Num a) => Expr a -> Expr a
derivative (Var c)           = Const 1
derivative (Const x)         = Const 0
--product rule (ab' + a'b)
derivative (a :*: b)         = (a :*: (derivative b)) :+:  (b :*: (derivative a)) -- product rule
 --power rule (xa^(x-1) * a')
derivative (a :^: (Const x)) = ((Const x) :*: (a :^: (Const $ x-1))) :*: (derivative a)
derivative (a :+: b)         = (derivative a) :+: (derivative b)
derivative (a :-: b)         = (derivative a) :-: (derivative b)
 -- quotient rule ( (a'b - b'a) / b^2 )
derivative (a :/: b)         = ((derivative a :*: b) :+: (negate' (derivative b :*: a)))
                               :/:
                               (b :^: (Const 2))
derivative expr              = error "I'm not a part of your system!" -- unsupported operation

ddx :: (Floating a, Eq a) => Expr a -> Expr a
ddx = fullSimplify . derivative

{-
strToExp' :: (Num a, Floating a) =>  String -> Expr a
strToExp' [x]
    | isDigit x = Const (fromIntegral $ digitToInt x)
    | otherwise = Var x
strToExp' [x,y]
    | isDigit x && isDigit y = (Const (((fromIntegral $ digitToInt x) * 10) + ((fromIntegral $ digitToInt y)*1)))
    | otherwise = error "invalid expression"
strToExp' [x,y,z]
    | y == '+' = (strToExp [x]) :+: (strToExp [z])
    | y == '*' = (strToExp [x]) :*: (strToExp [z])
    | y == '/' = (strToExp [x]) :/: (strToExp [z])
    | y == '-' = (strToExp [x]) :+: (negate' $ strToExp [z])
    | y == '^' = (strToExp [x]) :^: (strToExp [z])
    | isDigit x && isDigit y && isDigit z  = (Const ((( fromIntegral $digitToInt x) * 100) + ((fromIntegral $ digitToInt y)*10) + (( fromIntegral $ digitToInt z)*1)))  --if 3 digit number
    | otherwise = error "invalid expression"
    where toD a = digitToInt a
strToExp' (x:y:z:w:xs)
    | isOper y = case y of
                  '+' -> strToExp [x] :+: strToExp (z:w:xs)
                  '*' -> strToExp [x] :*: strToExp (z:w:xs)
                  '/' -> strToExp [x] :/: strToExp (z:w:xs)
                  '-' -> strToExp [x] :-: (negate' $ strToExp (z:w:xs))
                  '^' -> strToExp [x] :^: strToExp (z:w:xs)
    | isOper z = case z of
                 '+' -> strToExp [x,y] :+: strToExp (w:xs)
                 '*' -> strToExp [x,y] :*: strToExp (w:xs)
                 '/' -> strToExp [x,y] :/: strToExp (w:xs)
                 '-' -> strToExp [x,y] :-: (negate' $ strToExp (w:xs))
                 '^' -> strToExp [x,y] :^: strToExp (w:xs)
    | isOper w = case w of
                 '+' -> strToExp [x,y,z] :+: strToExp xs
                 '*' -> strToExp [x,y,z] :*: strToExp xs
                 '/' -> strToExp [x,y,z] :/: strToExp xs
                 '-' -> strToExp [x,y,z] :-: (negate' $ strToExp xs)
                 '^' -> strToExp [x,y,z] :^: strToExp xs
    | otherwise = error "invalid expression"
    where isOper s= s `elem` "+*/-^"
-}
