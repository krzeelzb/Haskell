{-# LANGUAGE FlexibleInstances #-}
import Data.Char

class Intable a where
    toInt :: a -> Int
   
instance Intable [Char] where
    toInt x = read x::Int
  
instance Intable Int where
    toInt x = x

mySuperAdd :: (Intable a, Intable b) => a -> b -> Int
mySuperAdd x y = toInt x + toInt y

data Osoba = Osoba
    { imie :: String
    , nazwisko:: String
    , pesel :: String
        } deriving (Show)
instance Eq Osoba where
    (==) x y=(pesel x == pesel y)

instance Ord Osoba where
    (<=) x y= (pesel x<=pesel y)

first :: String -> [Osoba] -> Maybe Osoba
first x []=Nothing
first x y
    | x == pesel (head y) = Just (head y)
    | otherwise = first x (tail y)
    
    


szymon = Osoba "Szymon" "Bobek" "12345678901"
bobek = Osoba "S" "Bobek"  "12345678901"
zenon = Osoba "Zenon" "Adamczyk" "111111111"