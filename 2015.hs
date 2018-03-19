-- Informatics 1 Functional Programming
-- December 2015
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>), Property )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

--p :: [Int] -> Int
--p xs | sum xs <=0 =1
--     | otherwise = mod (sum xs) 12
p :: [Int] -> Int
p xs = mod (div duration 60) 12 + 1
     where duration = sum [ x | x <- xs, x>=0 ]
test1a =p [] == 1 && p [-30,-20] == 1 && p [20,-30,30,14,-20] == 2 && p [200,45] == 5 && p [60,-100,360,-20,240,59] == 12 && p [60,-100,360,-20,240,60] == 1
-- 1b

q :: [Int] -> Int
q xs |ff xs<=0 = 1
     | otherwise = mod (ff xs ) 12
ff :: [Int]->Int
ff []= 0
ff (x:xs)= x+ff xs 

-- 1c

r :: [Int] -> Int
r xs|foldr (+) 0 xs <= 0= 1
    |otherwise = mod (foldr (+) 0 xs) 12 

-- Question 2

-- 2a

f :: String -> String
f []=[]
f xs= [x|(x,y)<-zip xs (tail xs),x/=y] ++[last xs]

-- 2b

g :: String -> String
g ""=[]
g [x] =  [x]
g (x:y:xs) |x==y =   g (y:xs) 
           |otherwise = x:g(y:xs)               
-- Question 3

data Regexp = Epsilon
            | Lit Char
            | Seq Regexp Regexp
            | Or Regexp Regexp
        deriving (Eq, Ord)

-- turns a Regexp into a string approximating normal regular expression notation

showRegexp :: Regexp -> String
showRegexp Epsilon = "e"
showRegexp (Lit c) = [toUpper c]
showRegexp (Seq r1 r2) = "(" ++ showRegexp r1 ++ showRegexp r2 ++ ")"
showRegexp (Or r1 r2) = "(" ++ showRegexp r1 ++ "|" ++ showRegexp r2 ++ ")"

-- for checking equality of languages

equal :: Ord a => [a] -> [a] -> Bool
equal xs ys = sort xs == sort ys

-- For QuickCheck

instance Show Regexp where
    show  =  showRegexp

instance Arbitrary Regexp where
  arbitrary = sized expr
    where
      expr n | n <= 0 = oneof [elements [Epsilon]]
             | otherwise = oneof [ liftM Lit arbitrary
                                 , liftM2 Seq subform subform
                                 , liftM2 Or subform subform
                                 ]
             where
               subform = expr (n `div` 2)



r1 = Seq (Lit 'A') (Or (Lit 'A') (Lit 'A'))   -- A(A|A)
r2 = Seq (Or (Lit 'A') Epsilon)
         (Or (Lit 'A') (Lit 'B'))             -- (A|e)(A|B)
r3 = Seq (Or (Lit 'A') (Seq Epsilon
                            (Lit 'A')))
         (Or (Lit 'A') (Lit 'B'))             -- (A|(eA))(A|B)
r4 = Seq (Or (Lit 'A')
             (Seq Epsilon (Lit 'A')))
         (Seq (Or (Lit 'A') (Lit 'B'))
              Epsilon)                        -- (A|(eA))((A|B)e)
r5 = Seq (Seq (Or (Lit 'A')
                  (Seq Epsilon (Lit 'A')))
              (Or Epsilon (Lit 'B')))
         (Seq (Or (Lit 'A') (Lit 'B'))
              Epsilon)                        -- ((A|(eA))(e|B))((A|B)e)
r6 = Seq (Seq Epsilon Epsilon)
         (Or Epsilon Epsilon)                 -- (ee)(e|e)

-- 3a

language :: Regexp -> [String]
language (Or x y )= language x ++ language y
language (Lit x)= [[x]]
language (Epsilon ) = [[]]
--language (Seq xs ys )= fff $ concat (map (\x-> map (x++) (language ys)) $ language xs )
--fff :: [String]->[String]
--fff []=[]
--fff (x:xs)|elem x xs = fff xs

language (Seq xs ys )= nub [x ++y|x<-language xs,y<-language ys]
-- 3b
--map (++) (language y)
simplify :: Regexp -> Regexp
simplify (Seq xs ys )|simplify xs == Epsilon = simplify ys 
                     |simplify ys == Epsilon =simplify xs
                     |otherwise =Seq ( simplify xs) (simplify ys)
simplify (Or xs ys) |simplify xs== simplify ys = simplify xs
                    |otherwise =  Or (simplify xs) (simplify ys) 
simplify (Lit r) = Lit r
simplify Epsilon = Epsilon

