-- Informatics 1 - Functional Programming 
-- Tutorial 5
--
-- Due: the tutorial of week 7 (3/4 November)


import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )

-- Warmup exercises

-- The datatype 'Fruit'
data Fruit = Apple String Bool
           | Orange String Int

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange "Seville" 12,
          Apple "Granny Smith" False,
          Apple "Braeburn" True,
          Orange "Sanguinello" 10]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

-- 1.
isBloodOrange :: Fruit -> Bool
isBloodOrange (Orange "Tarocco" a) =True 
isBloodOrange (Orange "Moro" a) =True 
isBloodOrange (Orange "Sanguinello" a) =True
isBloodOrange _ =False 
-- 2.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments []=0
bloodOrangeSegments (x:xs) |isBloodOrange x=f x + bloodOrangeSegments xs
                           | otherwise = bloodOrangeSegments xs
f ::Fruit -> Int
f (Orange a b)= b

-- 3.
worms :: [Fruit] -> Int
worms xs= length[1|x<-xs,ba x]

ba::Fruit -> Bool
ba (Apple _ True) =True
ba _ =False
-- Implementing propositional logic in Haskell
-- The datatype 'Prop'

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]


-- Functions for handling Props

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)        =  x
showProp (F)            =  "F"
showProp (T)            =  "T"
showProp (Not p)        =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)      =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)      =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)      =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q)      =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"
-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  not (eval e p) || eval e q
eval e (p :<->: q)    =  (not (eval e p) || eval e q) &&  ( not (eval e q) || eval e p)
-- retrieves the names of variables from a proposition - 
--  NOTE: variable names in the result must be unique
names :: Prop -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)      =  nub (names p ++ names q)
names (p :<->: q)      =  nub (names p ++ names q)
-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a proposition is satisfiable
satisfiable :: Prop -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


-- Exercises ------------------------------------------------------------

-- 4.
p1 = (Var "P" :|: Var "Q" ):&:(Var "P" :&: Var "Q")
p2 =(Var "P" :|: Var "Q" ):&:(Not (Var "P") :&:Not (Var "Q"))
p3 = (Var "P" :&: (Var "Q" :|: Var "R")):&:(((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P" ) :|: Not (Var "Q"))))


-- 5. 
tautology :: Prop -> Bool
tautology a = and [eval e a| e<-envs (names a)] 

prop_taut1 :: Prop -> Bool
prop_taut1 a = tautology a || satisfiable (Not a)

prop_taut2 :: Prop -> Bool
prop_taut2 a= not(satisfiable a) || not(tautology (Not a))


-- 6.
p4 = (Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q")
p5 = (Var "P" :->: Var "Q") :&: (Var "P" :&: Not ( Var "Q"))
p6 =  (Var "P" :<->: Var "Q") :&: ((Var "P" :&: Not( Var "Q")) :|: (Not (Var "P") :&: Var "Q"))


-- 7.
equivalent :: Prop -> Prop -> Bool
equivalent a b  = and (zipWith (==) [ eval e a | e <- envs (names (a :|: b)) ] [ eval e b | e <- envs (names (a :|: b)) ])
equivalent' :: Prop -> Prop -> Bool
equivalent' a b =  tautology (a :<->: b)

prop_equivalent :: Prop -> Prop -> Bool
prop_equivalent a b =  equivalent a b == equivalent' a b


-- 8.
subformulas :: Prop -> [Prop]
subformulas a = nub $ subformulas1 a 
subformulas1 :: Prop -> [Prop]
subformulas1 (Var a) =  [Var a] 
subformulas1  (F) = [F] 
subformulas1  (T) = [T]  
subformulas1 (Not a) =  (Not a ) : subformulas a 
subformulas1 (a :|: b) =  (a :|: b) : subformulas a ++ subformulas b
subformulas1 (a :&: b) =  (a :&: b) : subformulas a ++ subformulas b
subformulas1 (a :<->: b) =  (a :<->: b) : subformulas a ++ subformulas b
subformulas1 (a :->: b) =  (a :->: b) : subformulas a ++ subformulas b


-- Optional Material

-- 9.
-- check for negation normal form
isNNF :: Prop -> Bool 
isNNF (Var a) =  True 
isNNF  (F) = True 
isNNF  (T) = True 
isNNF (Not a) = isa a 
isNNF (a :|: b) =   isNNF a && isNNF b
isNNF (a :&: b) = isNNF a && isNNF b
isNNF (a :<->: b) =  False
isNNF (a :->: b) =  False

isa ::Prop -> Bool
isa (Var a) = True
isa _ = False
-- 10.
-- convert to negation normal form
toNNF :: Prop -> Prop
toNNF (Var c)=Var c
toNNF (F)=F
toNNF (T)=T
toNNF (a :|: b) = toNNF a :|: toNNF b  
toNNF (a :&: b) = toNNF a :&: toNNF b
toNNF (Not (a :&: b))= toNNF((Not a):|:(Not b) )
toNNF (Not (a :|: b))=toNNF( (Not a):&:(Not b))
toNNF (a :->: b) =toNNF ( (Not a) :|: b)
toNNF (a :<->: b)  = toNNF ((a :->: b) :&: ( b :->: a))
toNNF (Not (Not a))  = toNNF a
toNNF (Not a)=Not a

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Prop -> Bool
prop_NNF1 p  =  isNNF (toNNF p)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Prop -> Bool
prop_NNF2 p  =  equivalent p (toNNF p)


-- 11.
-- check whether a formula is in conj. normal form
isCNF :: Prop -> Bool
isCNF a = isNNF a && isCNF1 a 
isCNF1 :: Prop -> Bool
isCNF1 a = and (map is (toc a))
is (Var a) = True
is (a :&: b)= False
is (T)=True
is (F)=True
is (a:|: b)= is a && is b
is (Not a)=True

 
toc :: Prop -> [Prop]
toc (a :&: b ) = a:toc b
toc a=[a]
-- 13.
-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Prop]] -> Prop
listsToCNF  = undefined


-- 14.
-- transform a CNF formula into a list of lists
listsFromCNF :: Prop -> [[Prop]]
listsFromCNF = undefined


-- 15.
-- transform an arbitrary formula into a list of lists
toCNFList :: Prop -> [[Prop]]
toCNFList = undefined



-- convert to conjunctive normal form
toCNF :: Prop -> Prop
toCNF p  =  listsToCNF (toCNFList p)

-- check if result of toCNF is equivalent to its input
prop_CNF :: Prop -> Bool
prop_CNF p  =  equivalent p (toCNF p)




-- For QuickCheck --------------------------------------------------------

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)


-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"
