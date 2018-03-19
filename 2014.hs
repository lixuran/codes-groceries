-- Informatics 1 Functional Programming
-- December 2014
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, liftM3, used below
import Data.Char

-- Question 1

-- 1a

f :: [Int] -> Bool
f = undefined

-- 1b

g :: [Int] -> Bool
g = undefined

-- Question 2

-- 2a

p :: [Int] -> Int
p = undefined

-- 2b

q :: [Int] -> Int
q = undefined

-- 2c

r :: [Int] -> Int
r = undefined

-- Question 3

data Expr = X
          | Const Int
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | IfZero Expr Expr Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :-: q)  =  "(" ++ showExpr p ++ "-" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"
showExpr (p :/: q)  =  "(" ++ showExpr p ++ "/" ++ showExpr q ++ ")"
showExpr (IfZero p q r)  = "(if " ++ showExpr p ++ "=0 then "
                                  ++ showExpr q ++ " else "
                                  ++ showExpr r ++ ")"

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM2 (:+:) subform2 subform2
                                       , liftM2 (:-:) subform2 subform2
                                       , liftM2 (:*:) subform2 subform2
                                       , liftM2 (:/:) subform2 subform2
                                       , liftM3 (IfZero) subform3 subform3 subform3
                                       ]
                 where
                   subform2  =  expr (n `div` 2)
                   subform3  =  expr (n `div` 3)

-- 3a

eval :: Expr -> Int -> Int
eval (a :+: b) x=eval a x+eval b x 
eval (a :-: b) x= eval a x -eval b x
eval (a :*: b) x= eval a x *eval b x 
eval (a :/: b) x|eval b x/=0 = div (eval a x) (eval b x)
                | otherwise = error "hhh"  
eval (Const a) x=a 
eval (IfZero a b c) x|eval a x== 0 = eval b x
                     | otherwise = eval c x
eval X x= x
-- 3 b

protect :: Expr -> Expr
protect (a :+: b) =protect a :+: protect b
protect (a :-: b) = protect a :-: protect b
protect (a :*: b) = protect a :*: protect b
protect (a :/: b)= IfZero (protect b) (Const maxBound) (protect a :/: protect b)
protect (Const a) =Const a 
protect (IfZero a b c) = IfZero (protect a) (protect b) (protect c)
protect X =X

trickytest = X :/: (X :/: X)
test3b = eval (protect trickytest) 0 == 0
prop3 p n = eval p n == eval p n
check3 = quickCheck prop3
prop3' p n = eval (protect p) n == eval (protect p) n
check3' = quickCheck prop3'
