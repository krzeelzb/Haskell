-- {-# LANGUAGE FlexibleInstances #-}
import Data.List (sortBy)
import Text.Read (readMaybe) 
import Data.Maybe
import Data.Char
import Data.Map (Map)
import qualified Data.Map


data Student = Student {id :: Int, name :: String, dateOfBirth :: String} deriving(Show)
data Score = Score {studentId :: Int, course1Score :: Int, course2Score :: Int, course3Score :: Int} deriving (Show)



-- zadanie {1,2,3}

data StudentWithScores = StudentWithScores
    {s_id :: Int
    , s_name :: String
    , s_dateOfBirth :: String
    , s_course1Score :: Int
    , s_course2Score :: Int
    , s_course3Score :: Int
    } deriving (Eq, Show)


--data StudentWithScores  = StudentWithScores {} deriving (Student, Eq, Show)

--4
class Comp a where
    -- (==) :: a -> a -> Bool
    -- (>=) :: a -> a -> Bool
    -- (<=) :: a -> a -> Bool
    -- (>) :: a -> a -> Bool
    -- (<) :: a -> a -> Bool
    -- sortBirth :: [a] -> [a]
    -- sortName :: [a] -> [a]
    -- sortScore :: [a] -> [a]

instance Comp Student where
    -- (==) x y = ((s_dateOfBirth x) Prelude.== (s_dateOfBirth y))
    -- (>=) x y = ((s_dateOfBirth x) Prelude.>= (s_dateOfBirth y))
    -- (<=) x y = ((s_dateOfBirth x) Prelude.<= (s_dateOfBirth y))
    -- (>) x y = ((s_dateOfBirth x) Prelude.> (s_dateOfBirth y))
    -- (<) x y = ((s_dateOfBirth x) Prelude.< (s_dateOfBirth y))
    -- sortBirth = sortBy (comparing s_dateOfBirth)
    -- sortName = sortBy (comparing s_name)     

-- instance Comp Score where
--     sortScore = sortBy (comparing course1Score)



sortByDate :: [Student] -> [Student]
sortByDate = sortBy cmp 
    where cmp (Student _ _ date) (Student _ _ date2) = compare date2 date

sortBySum :: [Score] -> [Score]
sortBySum = sortBy cmp
    where cmp b a = compare (course1Score a + course2Score a + course3Score a) (course1Score b + course2Score b + course3Score b)


students :: [Student]
students = [Student 26453 "Kristalee Copperwaite" "2000",Student 33596 "Roeberta Naden" "1997", Student 26413 "Kristalee Copperaite" "2001", Student 32596 "Roebert Naden" "1999"]

studentsWithScores :: [StudentWithScores]
studentsWithScores = [StudentWithScores 1111 "Michas" "1997" 5 5 5, StudentWithScores 2222 "Elusia" "1997" 6 6 6]

scores :: [Score]
scores = [Score 2 1 1 1, Score 4 86 85 87]

--5
toStudentWithScores :: Student -> Score -> Maybe StudentWithScores
toStudentWithScores (Student sid sname sdate) (Score scid scc1 scc2 scc3) = if sid == scid then Just $ StudentWithScores sid sname sdate scc1 scc2 scc3 else Nothing

st = Student 1 "Ela" "1997"
scr = Score 1 5 4 5 

--6
findById :: [Score] -> Int ->[Score]
findById tab id = filter (\b -> studentId b == id) tab

--10

mapToJoin :: Student -> [Score] -> [Maybe StudentWithScores] 
mapToJoin student scores = map (toStudentWithScores student) scores

-- 11
joinStep1 :: [Student] -> [Score] -> [(Student, [Score])]
joinStep1 students scores = map (\student -> (student, scores)) students

-- 12
joinStep2 :: [(Student, [Score])] -> [[Maybe StudentWithScores]]
joinStep2  tuples = map (\(student, scores) -> mapToJoin student scores) tuples

-- 13
joinStep3 :: [[Maybe StudentWithScores]] -> [StudentWithScores]
joinStep3 maybeListOfLists = map (\maybeList -> getJust $ head $ filter isJust maybeList) maybeListOfLists

getJust:: Maybe a -> a
getJust (Just x) = x

-- 14
join :: [Student] -> [Score] -> [StudentWithScores]
join students scores = joinStep3 $ joinStep2 $ joinStep1 students scores
