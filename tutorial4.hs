-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (27/28 Oct)

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

low :: String -> String
low  []=[]
low (x:xs) |isUpper x = toLower x :low xs
           | otherwise = x : low xs 
-- 1.
sameString :: String -> String -> Bool
sameString s s2 = and $   (length s == length s2):zipWith (==) (low s) ( low s2)


-- 2.
prefix :: String -> String -> Bool
prefix "" s2 =True
prefix s "" = False
prefix s s2 = and $(length s<= length s2) :(zipWith (==) (low s) ( low s2)) 
prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
		         prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str
        
        
-- 3.
contains :: String -> String -> Bool
contains s []= False
contains s s2 = prefix s s2 || contains s (tail s2)  

prop_contains :: String -> Int -> Int -> Bool
prop_contains = undefined 


-- 4.
takeUntil :: String -> String -> String
takeUntil s []=[]
takeUntil s (x:xs) |not (prefix s (x:xs))=  x: takeUntil s xs 
                   | otherwise = []

dropUntil :: String -> String -> String
dropUntil s []=[]
dropUntil s (x:xs) |not (prefix s (x:xs))=  dropUntil s xs 
                   | otherwise = drop (length s) (x:xs)


-- 5.
split :: String -> String -> [String]
--split "" ""=[""]
split "" _ = error "shit"
split s []=[]
split s s2 =a : split s (drop (length a + length s) s2)  
           where a = takeUntil s s2
reconstruct :: String -> [String] -> String
--reconstruct s ss = foldr (++ [s] ++) "" ss
reconstruct s []=[]
reconstruct s (a:ss) = reconstruct s ss ++ s++a
prop_split ::  Char->String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep
--prop_split sep str = reconstruct sep (split sep str) `sameString` str
 

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML a = tail $ split "<a href=\"" a 


testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks

-- 7.
takeEmails :: [Link] -> [Link]
takeEmails a = filter (('m'==).head ) a


-- 8.
link2pair :: Link -> (Name, Email)
link2pair a = (  takeUntil "<" $ dropUntil ">" $ dropUntil "mailto" a,init $ takeUntil ">" $ dropUntil "mailto:" a)


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML a = map link2pair $ takeEmails(linksFromHTML a) 

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail n xs =[x|x<-xs, contains n $ fst x ] 


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML h n =  findEmail n $ emailsFromHTML h

-- Optional Material

-- 12.
hasInitials :: String -> Name -> Bool
hasInitials a b= a ==( map head $ split " " b  )

-- 13.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML f a = [x|x<- emailsFromHTML a , f $ fst x ]

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML s a = emailsByMatchFromHTML (hasInitials s) a 

-- 14.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria s =  head s == 'a'
-- myCriteria s =(or $ map (==x) "DS")
emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML a = emailsByMatchFromHTML myCriteria a 

-- 15
ppAddrBook :: [(Name, Email)] -> String
--ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
ppAddrBook addr = concat[(reconstruct ", " $ split " " n) ++ (replicate ( a - (length n)+1) ' ') ++ e ++ "\n"  |(n,e)<-addr]
                  where a = maximum $ map length $ map fst addr
                       -- b = reconstruct ", " $ split " " n
