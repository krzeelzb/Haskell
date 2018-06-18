import Data.Char
import Data.String
import Test.HUnit

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

testTree :: Tree Int
testTree = Node 5 (Node 3 Empty (Node 4 Empty Empty)) (Node 10 (Node 7 Empty Empty) (Node 17 Empty Empty))


-- insert (wstawienie elementu)
insert :: (Ord a) => a -> Tree a -> Tree a
insert element Empty = Node element Empty Empty
insert element (Node parent left right)
    | element <= parent = Node parent (insert element left) right
    | element > parent = Node parent left (insert element right)

-- empty (sprawdzanie czy drzewo jest puste)
empty:: Tree a ->Bool
empty Empty=True
empty _=False

-- search (sprawdzanie czy element jest w drzewie)
search:: (Ord a) => (Tree a) -> a -> Bool
search Empty _=False
search (Node parent left right) element
    | element == parent= True
    | element < parent = search left element
    | element > parent= search right element

-- toString (wypisującą drzewo w postaci „a(b(d ,e),c(,f(g,)))” )

toString Empty=putStr " ,"
toString (Node parent left right)= do
    putStr( show parent ++ "(") 
    toString left
    putStr( "),(")
    toString right
    putStr(")")

-- leaves ( zwracającą listę liści )
leaves:: Ord a =>Tree a -> [a]
leaves Empty=[]
leaves (Node head Empty Empty)= [head]
leaves (Node head left Empty)=leaves left
leaves (Node head Empty right)=leaves right
leaves (Node head left right)= leaves left ++ leaves right

-- nnodes (podającą ilość węzłów)
nnodes:: Tree a ->Int
nnodes Empty =0
nnodes (Node head left right)= 1+ nnodes left + nnodes right

-- nsum (zliczającą sumę wartości w węzłach)
nsum Empty =0
nsum (Node head left right)= head+ nsum left + nsum right

-- isBalanced (sprawdzanie czy drzewo jest zrównoważone)
isBalanced :: Tree a -> Bool
isBalanced (Node _ left right) = abs(height left - height right) <= 1 && isBalanced left && isBalanced right
isBalanced Empty = True

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) =  1 + max (height left) (height right) 

-- traverse{VLR, LVR, LRV, VRL, RVL, RLV} - (przejście po elementach drzewa na wszystkie możliwe sposoby, funkcja ma zwracać listę węzłów)
vlr Empty = []
vlr(Node v left right)=
  [v] ++ vlr left ++ vlr right

lvr Empty = []
lvr (Node v left right)=
  lvr left ++ [v] ++ lvr right
  
lrv Empty = []
lrv (Node v left right)=
  lrv left ++ lrv right ++ [v]
  
vrl Empty = []
vrl(Node v left right)=
  [v] ++ vrl right ++ vrl left
  
rvl Empty = []
rvl (Node v left right)=
  rvl right ++ [v] ++ rvl left
  
rlv Empty = []
rlv (Node v left right)=
    rlv right ++ rlv left ++ [v]

-- tmap (odpowiednik funkcji map dla drzewa)
-- tmap :: (a->b) -> Tree a -> Tree b
-- tmap action Empty = Empty
-- tmap action (Node parent left right) = Node result mapLeft mapRight
--      where 
-- 	  result = action parent
-- 	  mapLeft = tmap action left
--       mapRight = tmap action right
      
-- getLevel
getLevel x Empty = []
getLevel 0 (Node x _ _) = [x]
getLevel x (Node v left right)= getLevel (x-1) left ++ getLevel (x-1) right

-- merge (łączenie dwóch drzew)

-- chyba nie do końca dobrze xd
-- remove (usuwanie elementu)*
-- remove :: (Ord a) => Tree a -> a -> Tree a
-- remove Empty _=Empty
-- remove (Node parent left right) element
--     |element == parent = removeElement (parent left right)
--     |element < parent = Node parent (remove left element) right
--     |element > parent = Node parent left (remove right element)

-- -- removeElement:: (Ord a) => Tree a -> Tree a 
-- removeElement (Node parent Empty right)=right
-- removeElement (Node parent left Empty)=left
-- removeElement (Node parent left right)=Node parent left2 right
--     where left2=leftistElement right

-- -- leftistElement :: (Ord a) => Tree a -> a
-- leftistElement (Node parent Empty  _) = parent
-- leftistElement (Node  _ left _) = leftistElement left

testlist = TestList ["Leaves" ~: leaves testTree ~?= [4,7,17]
    , "Nnodes" ~: nnodes testTree  ~?=6
    , "Nsum" ~: nsum testTree ~?= 46
    , "Empty? Bad test" ~: empty testTree ~?= True
                    ]
main :: IO ()
main = do
  runTestTT testlist
  return ()