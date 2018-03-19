-- Informatics 1 - Functional Programming 
-- Tutorial 6
--
-- Week 8 - Due: 10/11 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1
 
-- 1a. split
split :: Command -> [Command]
split (a :#: Sit) =split a
split (Sit :#: a) =split a 
split (a :#:b)= split a ++ split b  
split a=[a]
-- 1b. join
join :: [Command] -> Command
join [x]=x
join (x:xs) = x :#: join xs 

-- 1c  equivalent
equivalent ::Command -> Command ->Bool
equivalent a b= and (zipWith (==)(split a) (split b))

-- 1d. testing join and split
prop_split_join ::Command ->Bool
prop_split_join a= equivalent a (join $ split a)
prop_split :: Command-> Bool
--prop_split a=filter (elem [Sit,:&:] )$ split a  
prop_split x =and $map f $ split x
f::Command->Bool
f Sit = False
f (a :#: b)= False
f _ =True
-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy 1 b=b
copy a b = b :#: copy (a-1) b

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon a = copy 5 (Go a :#: Turn 72.0) 

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon a b = copy b (Go a :#: Turn c) 
             where c = 360.0 / (fromIntegral b)    


-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral a 1 c d = Go a :#: Turn d  
spiral a b c d = Go a :#: Turn d :#: spiral (a+c) (b-1) c d 


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise x |equivalent (join (aa (split x))) x=join (aa (split x))
           | otherwise =optimise (join (aa (split x)))
aa :: [Command]-> [Command]
aa []=[]
aa (Go b :Go a :xs) =aa (Go (a+b): xs)
aa (Go 0.0:xs)=aa xs 
aa (Turn b :Turn a :xs) =aa (Turn (a+b) : xs)
aa (Turn 0.0:xs)=aa xs 
aa (x:xs)=x:aa xs

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead a= f a
           where f 0 = GrabPen red :#: Go 10
                 f x = g (x-1) :#: n :#: f (x-1) :#: n :#: g (x-1)
                 g 0 =  GrabPen blue :#: Go 10
                 g x = f (x-1) :#: p :#: g (x-1) :#: p :#: f (x-1)
                 n = Turn 60.0
                 p= Turn (-60.0) 
-- 6. snowflake
snowflake :: Int -> Command
snowflake a = copy 3 (f a :#: p :#: p) 
            where f 0 =Go 10
                  f x = f (x-1) :#: n :#: f (x-1) :#: p :#: p :#: f (x-1) :#: n :#: f (x-1)
                  n = Turn 60.0
                  p= Turn (-60.0)
--f [] =True
--f [ x]= True 
--f xs= and [if even x then div x 2 == y else 3*x +1 ==y|(x,y)<-zip xs (tail xs)]
-- 7. hilbert

--rewrite: l → +rf-lfl-fr+
--r → -lf+rfr+fl
hilbert :: Int -> Command
hilbert a = l a 
          where n = Turn 90.0
                p = Turn (-90.0) 
                l 0 = GrabPen red 
                l a = n :#: r (a-1) :#: f  :#: p :#: l (a-1) :#: f :#: l (a-1) :#: p :#: f  :#: r (a-1) :#: n
                r 0 =  GrabPen blue
                r a = p :#: l (a-1) :#: f  :#: n :#: r(a-1) :#: f :#: r (a-1) :#: n :#: f  :#: l (a-1) :#: p
                f = Go 10

br :: Int -> Command 
br a = g a 
     where n = Turn 22.5
           p = Turn (-22.5)
           g 0 = GrabPen red :#: Go 10
           g x = f (x-1) :#: p :#:  Branch ( Branch (g (x-1)) :#: n :#: g (x-1)) :#: f (x-1) :#: Branch (n :#: f (x-1) :#: g (x-1)) :#: p :#: g (x-1)
           f 0 =  GrabPen blue :#: Go 10
           f x = f (x-1) :#: f (x-1)
           
