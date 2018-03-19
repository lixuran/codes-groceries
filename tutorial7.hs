-- Informatics 1 Functional Programming
-- Tutorial 7
--
-- Due: 17/18 November

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = maximum.map length.map fst.map snd $ xs 

formatLine :: Int -> (Barcode, Item) -> String
formatLine a xs= fst xs ++"..."++ fst (snd xs) ++ replicate (a - (length.fst.snd) xs)'.'++snd (snd xs)

showCatalogue :: Catalogue -> String
showCatalogue xs = concat[formatLine (longestProductLen a) x ++ "\n"| x<-a] 
                  where a = toList xs
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe a = Just (head a)

catMaybes :: [Maybe a] -> [a]
catMaybes xs = foldr (++) [] $ map maybeToList xs

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems xs a = catMaybes $ map (\x -> get x a) xs






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
--(1.97 secs, 784253656 bytes)
--Just ("Mens Travel Attache Personal Care Products","2 lb")
--(0.10 secs, 0 bytes)
-- getSample theDB
--"0015000765972"
--(0.02 secs, 0 bytes)
-- get it theDB
--Just ("GERBER TOILET LID LOCK","EACH")
--(0.30 secs, 0 bytes)
--ghc: out of memory (requested 2097152 bytes)
