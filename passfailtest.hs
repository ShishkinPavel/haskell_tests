import Data.Maybe


evalF :: Bool -> Bool -> Bool -> Bool -> Bool
evalF _ _ False _ = False
evalF _ _ _ _ = True


binMul :: [Integer] -> [Integer]
binMul [_] = []
binMul (x:xs) = x * head xs : (binMul xs)


sumMaximumFold1 :: [[Integer]] -> [Integer]
sumMaximumFold1 [] = [0]
sumMaximumFold1 [x] = [maximum x]
sumMaximumFold1 (x:xs) = maximum x : (sumMaximumFold1 (xs))

sumMaximumFold :: [[Integer]] -> Integer
sumMaximumFold [] = 0
sumMaximumFold [x] = maximum x
sumMaximumFold (x:xs) = sum(sumMaximumFold1 (x:xs))


rotateL :: Int -> [a] -> [a]
rotateL _ [] = []
rotateL n xs | n >= 0 = zipWith const (drop n (cycle xs)) xs
   | otherwise = reverse (zipWith const (drop (-n) (cycle (reverse xs))) xs)



multiply :: Int -> [a] -> [a]
multiply n xs = concatMap (replicate n) xs

data Empire = Rules [Colony]
            deriving Show

data Colony = Plain
            | Settlement Integer
            | Districts [Colony]
            deriving Show





totalPeople :: Empire -> Integer
totalPeople (Rules []) = 0
totalPeople (Rules ((Plain):xs)) = totalPeople (Rules xs)
totalPeople (Rules ((Districts ds) : xs)) = totalPeople (Rules ds) + totalPeople (Rules xs)
totalPeople (Rules ((Settlement s):xs)) = s + totalPeople (Rules xs)



findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findStrHelp pat str 0
  where
    findStrHelp _ [] _ = Nothing
    findStrHelp pat s@(x:xs) n
      | pat == (take (length pat) s) = Just n
      | otherwise = findStrHelp pat xs (n+1)


data Material = Bullet | Handgun | Bazooka | Stinger | Grenade
              deriving Eq
type Mission  = [Material]
type Prices a = Material -> a

mission01 :: Mission
mission01 = [Handgun, Stinger, Handgun] ++ replicate 336 Bullet

prices2021 :: Fractional a => Prices a
prices2021 Bullet = 0.125
prices2021 Handgun = 1024
prices2021 Stinger = 42000

missionCost :: Fractional a => Mission -> Prices a -> Material -> a
missionCost xs cena material = sum (map cena (filter (== material) xs))


isRightTriangle' :: Integer -> Integer -> Integer -> Bool
isRightTriangle' x y z = if x >= y && x >= z then pyt y z x else if y >= z then pyt x z y else pyt x y z where pyt a b c = a ^ 2 + b ^ 2 == c ^ 2

isNormTriangle :: Integer -> Integer -> Integer -> Bool
isNormTriangle 0 _ _ = False
isNormTriangle _ 0 _ = False
isNormTriangle _ _ 0 = False
isNormTriangle a b c = a + b > c || a + c > b || b + c > a



triangles :: [(Integer, Integer, Integer)]
triangles = [(a, b, c)|  c <-[1.. ],  b <- [1.. c], a <- [1.. b], a + b > c || a + c > b || b + c > a ]





