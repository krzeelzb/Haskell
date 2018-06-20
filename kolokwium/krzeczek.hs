{-# LANGUAGE FlexibleInstances #-}
import qualified Data.Map as M  
import Data.List
import Data.Char
import Test.HUnit
 
-- Elżbieta Krzeczek: _________________

-----------------------------------------------------------------------------

-- SEKCJA 1: PROSTE LISTY NA ROZGRZEWKĘ

-- [7.5%] ZADANIE 1.A
-- Napisz funkcję, która wyciąga elementy znajdujące się na nieparzystych
-- pozycjach w zadanej liście
-- Przykład:
-- ghci> nieparzyste [2, 5, 6, 13, 32]
-- [2, 6, 32]
--nieparzyste :: [Int] -> [Int]
nieparzyste :: [Int] -> [Int]
nieparzyste []=  []
nieparzyste [el] = [el]
nieparzyste (el1:x2:xs) = el1 : nieparzyste xs


--------------------------------------

-- [7.5%] ZADANIE 1.B
-- Napisz funkcję, która przyjmuje dwa argumenty: xmax, ymax
-- i zwraca listę par współrzędnych całkowitoliczbowych, które znajdują się
-- wewnątrz prostokąta, którego jednym z rogów jest (0,0),
-- a drugim (xmax, ymax)
-- Przykład:
-- ghci> wspolrzedne 1 1
-- [(0,0), (0,1), (1,0), (1,1)] 
-- ghci> wspolrzedne 1 2
-- [(0,0), (0,1), (0,2), (1,0), (1, 1), (1, 2)]
-- wspolrzedne :: Int -> Int -> [(Int, Int)]

-- wspolrzedne :: Int -> Int -> [(Int, Int)]
wspolrzedne xmax ymax= [ (a,b) | a <- [0..xmax] , b <- [0..ymax]]

-- -- wspolrzedne 0 0 = (0,0)
-- wspolrzedne xmax ymax = (xmax,ymax) : wspolrzedne (xmax-1,ymax-1) 


-----------------------------------------------------------------------------

-- SEKCJA 2: ROSE TREE

-- Dany jest typ reprezentujący Rose Tree (czyli drzewo, które może mieć
-- dowolną liczbę gałęzi w każdym z węzłów):
data Rose a = a :> [Rose a] deriving Show
-- (moglibyśmy to zapisać jako: data Rose a = Rose a [Rose a],
-- ale zapis z :> będzie czytelniejszy i wygodniejszy)

-- Dane jest również przykładowe drzewo dla jasności:
przyklad = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]


-- [5%] ZADANIE 2.1
-- Zdefiniuj funkcję, która oblicza rozmiar drzewa (ilość węzłów w drzewie)
-- Przykład:
-- ghci> rozmiar przyklad
-- -- 14


rozmiar :: Rose a -> Int
rozmiar (_ :> list) = 1 + sum ( map rozmiar list)

--------------------------------------

-- [5%] ZADANIE 2.2
-- Zdefiniuj funkcję, która oblicza liczbę liści w drzewie
-- Przykład:
-- ghci> liscie przyklad

liscie :: Rose a -> Int
liscie (_ :> [])= 1
liscie (_ :> list) = sum (map liscie list)

-- leaves:: Ord a =>Tree a -> [a]
-- leaves Empty=[]
-- leaves (Node head Empty Empty)= [head]
-- leaves (Node head left Empty)=leaves left
-- leaves (Node head Empty right)=leaves right
-- leaves (Node head left right)= leaves left ++ leaves right

--------------------------------------

-- [15%] ZADANIE 2.3
-- Znasz dobrze funkcję map (a jeżeli nie to się nie przyznawaj!)
-- chcielibyśmy coś podobnego mieć dla drzewa = mieć możliwość wywołania
-- funkcji dla każdego elementu drzewa i otrzymania drzewa z wynikami.
-- Taka funkcjonalność w Haskellu jest realizowana przez type-class Functor,
-- który definiuje funkcję fmap, która przyjmuje funkcję i jakąś strukturę

-- Zwykły map działa dla list, więc w przypadku definiowania Functor dla
-- listy moglibyśmy po prostu wywołać map:
-- instance Functor [] where
--    fmap = map

-- W przypadku naszego drzewa będzie to tylko trochę trudniejsze :)
-- Przykłady:
-- ghci> fmap (*2) (1 :> [2 :> [], 3 :> []])
-- (2 :> [4 :> [], 6 :> []]) 
-- ghci> fmap (+1) (1 :> [])
-- (2 :> [])

instance Functor Rose where
    -- fmap f :> list = Node (f x) (fmap f leftsub) (fmap f rightsub)
    fmap = undefined

    -- tmap :: (a->b) -> Tree a -> Tree b
    -- tmap f Empty = Empty
    -- tmap f (Node v left right) = Node result mapLeft mapRight
    --      where 
    --       result = f v
    --       mapLeft = tmap f left
    --       mapRight = tmap f right

    -- liscie (_ :> list) = sum (map liscie list)

--     instance Functor Tree where  
--         fmap f [] = EmptyTree  
--         fmap f :> list = Node (f x) (fmap f leftsub) (fmap f rightsub)
-- --------------------------------------

-- [15%] ZADANIE 2.4
-- Napisz funkcję, która spłaszcza nasze drzewo do listy. Algorytm przechodzenia
-- po drzewie jest dowolny.
-- Przykład dla przeszukiwania drzewa w głąb (DFS):
-- ghci> splaszcz przyklad
-- [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
splaszcz :: Rose a -> [a]
splaszcz = undefined

-- vlr Empty = []
-- vlr(Node v left right)=
--   [v] ++ vlr left ++ vlr right

-----------------------------------------------------------------------------

-- SEKCJA 3: FOLD

-- [20%] ZADANIE 3
-- Napisz funkcję mojaMapa korzystajac z foldr/foldl.
-- Funkcja powinna mieć taką samą funkcjonalność jak wbudowana funkcja map
mojaMapa :: (a -> b) -> [a] -> [b]
mojaMapa f xs=foldr(\x acc-> f x:acc ) [] xs

-----------------------------------------------------------------------------

-- SEKCJA 4: TESTY

-- [5*5%] Zdefiniuj testy jednostkowe do wybranych 5 zadań z kolokwium.
-- Dla każdego zadania powinny być dwa testy. Jeden z nich może być przykładem
-- zaczerpniętym z opisu zadania, drugi test wymyśl samodzielnie.
-- Zdefiniuj również funkcję uruchomTesty, która uruchamia zestaw wszystkich
-- przygotowanych przez ciebie testów.
przyklad2= 9 :> [10 :> []]
testlist2 = TestList [ "testMojaMapa1" ~: mojaMapa (*3) [1..3] ~?= [3,6,9],
                     "testMojaMapa2 Failure!" ~: mojaMapa (+1) [1..3] ~?= [3,6,9],
                     "testNieparzyste1" ~: nieparzyste [1,3,5,6] ~?= [1,5],
                     "testNieparzyste2 Failure" ~: nieparzyste [1,3,5,6] ~?= [1,3,5],
                     "testrozmiar1" ~: rozmiar przyklad ~?=14  ,
                     "testrozmiar2 Failure" ~: rozmiar przyklad2 ~?=0,
                     "testliscie1" ~: liscie przyklad ~?=6  ,
                     "testliscie2 Failure" ~: liscie przyklad2 ~?=0,
                    "testwspolrzedne1" ~: wspolrzedne 1 1 ~?= [(0,0), (0,1), (0,2), (1,0), (1, 1), (1, 2)],
                     "testwspolrzedne2 Failure" ~: wspolrzedne 1 4 ~?= [(0,0), (0,1), (0,2), (1,0), (1, 1), (1, 2)]
                    --  "testfmap1" ~: fmap (*2) (1 :> [2 :> [], 3 :> []])~?=(2 :> [4 :> [], 6 :> []]),
                    --  "testfmap2 Failure" ~: fmap (*2) przyklad2 ~?=(2 :> [4 :> [], 6 :> []])
                    --  "testwspolrzedne1" ~: wspolrzedne (1 1) ~?= [( , )] ,
                    --  "testwspolrzedne2 Failure" ~: wspolrzedne (1 4) ~?= [( , )]
                    --  "testPred2" ~: (10 >15) @? "Failure"              
                    ]

-- main :: IO ()
uruchomTesty  = do
  runTestTT testlist2
  return ()
