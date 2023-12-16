module Interpreter where 

import Lexer 
import Parser

isValue :: Expr -> Bool 
isValue BTrue = True 
isValue BFalse = True
isValue (Num _) = True 
isValue (Lam _ _ _) = True
isValue _ = False 

subst :: String -> Expr -> Expr -> Expr 
subst x n (Var v) = if (x == v) then
                      n 
                    else 
                      (Var v)
subst x n (Lam v t b) = Lam v t (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
subst x n (Or e1 e2) = Or (subst x n e1) (subst x n e2)
subst x n (If e1 e2 e3) = If (subst x n e1) (subst x n e2) (subst x n e3)
subst x n (Lt e1 e2) = Lt (subst x n e1) (subst x n e2)
subst x n (Le e1 e2) = Le (subst x n e1) (subst x n e2)
subst x n (Gt e1 e2) = Gt (subst x n e1) (subst x n e2)
subst x n (Ge e1 e2) = Ge (subst x n e1) (subst x n e2)
subst x n (Eqt e1 e2) = Eqt (subst x n e1) (subst x n e2)
subst x n (Diff e1 e2) = Diff (subst x n e1) (subst x n e2)
subst x n (Paren e) = Paren (subst x n e)
subst x n (Let v e1 e2) = Let v (subst x n e1) (subst x n e2)
subst x n e = e 

step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n) e) = Add (Num n) (step e)
step (Add e1 e2) = Add (step e1) e2 
step (And BFalse _) = BFalse 
step (And BTrue e) = e 
step (And e1 e2) = And (step e1) e2
step (Or BTrue _) = BTrue
step (Or BFalse e) = e
step (Or e1 e2) = Or (step e1) e2
step (If BFalse e1 e2) = e2 
step (If BTrue e1 e2) = e1 
step (If e e1 e2) = If (step e) e1 e2
step (Eqt (Num n1) (Num n2)) = if n1 == n2 then BTrue else BFalse
step (Eqt e1 e2) = Eqt (step e1) e2
step (Diff (Num n1) (Num n2)) = if n1 /= n2 then BTrue else BFalse
step (Diff e1 e2) = Diff (step e1) e2
step (Lt (Num n1) (Num n2)) = if n1 < n2 then BTrue else BFalse
step (Lt e1 e2) = Lt (step e1) e2
step (Le (Num n1) (Num n2)) = if n1 <= n2 then BTrue else BFalse
step (Le e1 e2) = Le (step e1) e2
step (Gt (Num n1) (Num n2)) = if n1 > n2 then BTrue else BFalse
step (Gt e1 e2) = Gt (step e1) e2
step (Ge (Num n1) (Num n2)) = if n1 >= n2 then BTrue else BFalse
step (Ge e1 e2) = Ge (step e1) e2
step (Paren e) = e
step (App (Lam x t b) e2) | isValue e2 = subst x e2 b 
                        | otherwise = (App (Lam x t b) (step e2))
step (App e1 e2) = App (step e1) e2
step (Let v e1 e2) | isValue e1 = subst v e1 e2 
                   | otherwise = Let v (step e1) e2
step e = error (show e)

eval :: Expr -> Expr 
eval e | isValue e = e 
       | otherwise = eval (step e)