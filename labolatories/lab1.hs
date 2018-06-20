import Data.List 

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
