{-# LANGUAGE FlexibleInstances #-}
import Data.Char
import Data.List 
import Data.List 
-- LAB 1..........
myHead [] = []
myHead (head:tail) = head 

myLength []=0
myLength (h:t)=1 + myLength t

take' 0 _ = []
take' x (h:t) = h : take' (x-1) t

map' _ []=[]
map' x (h:t)= x h : map' x t

plus [] y = y
plus (h1:t1) y = h1: plus t1 y

zipp:: [a] -> [b] -> [(a,b)]
zipp [] [] = []
zipp (h1:t1) (h2:t2) = (h1,h2) : zipp t1 t2

-- LAB 2..........
bakteria :: Int -> (Int,Int)
bakteria 0 = (1,1)
bakteria 1 = (1,3)
bakteria n = (snd(bakteria(n-1)),2*fst(bakteria(n-1))+snd(bakteria(n-1)))  

bakteriaa :: Int -> (Int,Int)
bakteriaa 0 = (2,1)
bakteriaa 1 = (1,5)
bakteriaa n = (snd(bakteriaa(n-1)),2*fst(bakteriaa(n-1))+snd(bakteriaa(n-1)))  


supercyfra :: Int -> Int
supercyfra n | n < 10 = n
	     | n>=10 =supercyfra ((n `quot` 10) + (n `mod` 10) )

usunduplikaty :: Eq a => [a] -> [a]
usunduplikaty [] = []
usunduplikaty [x]=[x]
usunduplikaty (h:t) = h: [k| k <-usunduplikaty (t), k/=h]


count:: Eq a => a -> [a] -> Int
count n [] = 0
count n (h:t) | n == h = 1 + count n t
              | otherwise = count n t

atLeast :: [Int] -> Int -> [Int]
atLeast list min = nub $ filter (\b-> count b list >= min) list

-- LAB 3..........


sum2c :: Int -> Int -> Int
sum2c m n = m + n

dodaj1 :: Int -> Int
dodaj1 = sum2c 1

dodaj1doListy :: [Int] -> [Int]
dodaj1doListy = map dodaj1

mojeLiczby = [f x | x <- lista, p x]
             where f = \a -> 2 * a          -- f mnoży liczbę razy 2
                   lista = [1..10]          -- lista początkowa
                   p = \b -> b `mod` 2 == 0 -- p wybiera liczby parzyste
 
mojeLiczby'=map (*2) $ filter (even) [1..10]

generatorOperator :: (lewa -> prawa -> wynik) -> lewa -> (prawa -> wynik)
-- funkcja przyjmuje operator, który jest typu (lewa -> prawa -> wynik)
-- oraz lewą część operatora i zwraca funkcję, która przyjmuje prawą część operatora i zwraca wynik
generatorOperator x y z = (x) y z
 
dodaj3 = generatorOperator (+) 3
podziel100 = generatorOperator (/) 100

reverse' :: String -> String
reverse' x =foldl (\a e -> e:a) "" x

reverse'' :: String -> String
reverse'' x =foldr (\e a -> a++[e]) "" x

policzISumuj :: (Int -> Int) -> Int -> Int -> Int
policzISumuj x y z= foldl (+) 0 (map (x) [y..z])

czyPiersza:: Int -> Int
czyPiersza x =foldr (+) 0 $ filter (\b -> x `mod` b == 0) [2..x-1]  

pierwsze :: [Int] -> [Int]
pierwsze x= filter (\b -> czyPiersza b == 0) x

co :: [Int] -> Int -> [Int]

co lista min=nub $filter (\b -> ((foldl (+) 0 $map (\a -> 1) $ filter (\x -> x==b) lista)) >= min )lista 

-- LAB 4..........

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