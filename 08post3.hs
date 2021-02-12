import Data.List

type PersonId = Integer
type PersonName = String
type InfectionId = Integer
type InfectionName = String

-- Do not add, remove or rearrange fields of these data definitions.
-- Do not add, remove or rename their constructors.
data TerminationType = Recovered | Dead
                       deriving (Eq, Show)

data Person = Person PersonId PersonName
              deriving Show

data Infection = Infection InfectionId PersonId InfectionName
                 deriving Show

data Termination = Termination InfectionId TerminationType
                   deriving Show

type Persons = [Person]
type Infections = [Infection]
type Terminations = [Termination]

-- TODO: Uncomment and implement these functions according to the assignment.

-- 1

deldup :: Eq a => [a] -> [a]
deldup xs = nub xs


need :: Infections -> InfectionName -> Infections
need ((Infection b c x):xs) a = filter (\(Infection _ _ x) -> x == a) ((Infection b c x):xs)
   
   
ider :: Infections -> [PersonId]
ider [] = []
ider ((Infection a b x):xs) = b:(ider xs)

countOfInfected :: Infections -> InfectionName -> Int
countOfInfected [] _ = 0
countOfInfected xs a = length $ deldup $ ider $ need xs a

-------------------------------------------------------------
-- 2

dead :: Terminations -> [InfectionId]
dead [] = []
dead ((Termination y x):xs) 
   | x == Dead = y:(dead xs)
   | otherwise = dead xs

-------------------------------------------------------------
-- 3

finder :: Termination -> InfectionId
finder (Termination a b) = a

activeCases :: Infections -> Terminations -> Infections
activeCases infs xs = 
   let allterm = map finder xs
   in filter (\(Infection x _ _) -> all (x /=) allterm) infs

-------------------------------------------------------------
-- 4


finderdie :: Infections -> [InfectionId] -> Infections
finderdie _ [] = []
finderdie [] _ = []
finderdie ((Infection a b x):xs) ys = filter (\(Infection a _ x) -> any (a ==) ys) ((Infection a b x):xs)

namer :: Infections -> [InfectionName]
namer [] = []
namer ((Infection a b x):xs) = x:(namer xs)


somebodyDied :: Infections -> Terminations -> [InfectionName]
somebodyDied infs ys = deldup $ namer $ finderdie infs (dead ys)

-------------------------------------------------------------
-- 5

countDead :: Infections -> Terminations -> [PersonId]
countDead _ [] = []
countDead [] _ = []
countDead xs ys = ider $ finderdie xs (dead ys)


allTheSame :: (Eq a) => [a] -> Bool
allTheSame [] = True
allTheSame xs = length (nub xs) == length xs


checkDeaths :: Infections -> Terminations -> Bool
checkDeaths [] [] = True
checkDeaths inf ter = allTheSame $ countDead inf ter

-------------------------------------------------------------
-- 6

nametoid :: Persons -> PersonId
nametoid ((Person x s):xs) = x 

persinf::  Infections -> PersonId -> Infections
persinf [] _ = []
persinf ((Infection a b c):bs) x = filter (\(Infection a b c) -> b == x) ((Infection a b c):bs)



diseases :: Persons -> Infections -> [(PersonName, [InfectionName])]
diseases [] _ = []

diseases ((Person a b):xs) inf = (b, namer $ persinf inf a):diseases xs inf

-- Data from the assignment examples. You can modify them arbitrarily. You
-- should test your solution on your own data.

pers :: Persons
pers = [Person 1 "Augustin"
       ,Person 2 "Baltazar"
       ,Person 42 "Ctirad"
       ,Person 128 "Zdenek"
       ,Person 5 "Drahoslav"
       ]

infs :: Infections
infs = [Infection 2020 1 "COVID"
       ,Infection 2019 42 "COVID"
       ,Infection 1 5 "COVID"
       ,Infection 5 128 "rymicka"
       ,Infection 3 5 "astma"
       ,Infection 2 1 "astma"
       ,Infection 128 5 "zapal plic"
       ]

ters :: Terminations
ters = [Termination 2020 Dead
       ,Termination 2 Recovered
       ,Termination 2019 Recovered
       ,Termination 128 Dead
       ]
