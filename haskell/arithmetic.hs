(?) cond a = if cond then (\x -> a) else (\x -> x)

-- проверка на простоту
isPrime :: Int -> Bool
isPrime n
        | n == 2 = True
        | n < 2 || even n = False
        | otherwise = all (\i -> n `mod` i /= 0) [3 .. n-1]


powModF :: Int -> Int -> Int -> Int
powModF a x p
        | a `mod` p == 0 = error "ошибка: a делиться на p"
        | not $ isPrime p = error "ошибка: p - составное число"
        | otherwise = foldl (\r nA -> r * nA `mod` p) 1 repA

    where newX = x `mod` (p-1)
          repA = take newX $ repeat a


-- возведенение в степень по модулю
powMod :: Int -> Int -> Int -> Int
powMod _ 0 _ = 1
powMod a power n = if even power
                   then result
                   else result * a `mod` n

    where newA = a * a `mod` n
          newPower = power `div` 2
          result = powMod newA newPower n


nod :: Int -> Int -> Int
nod r 0 = r
nod a b = nod b (a `mod` b)


nodExtend' :: Int -> Int -> [Int] -> [Int] -> (Int, Int, Int)
nodExtend' a b [v, vP] [u, uP] =
        if a `mod` b == 0
        then (v, u, b)
        else nodExtend' b r [newV, v] [newU, u]

    where (d, r) = divMod a b
          newV = vP - d * v
          newU = uP - d * u

nodExtend :: Int -> Int -> (Int, Int, Int)
nodExtend a b = nodExtend' a b [0, 1] [1, 0]


inverseMod :: Int -> Int -> Int
inverseMod a b =
        if d /= 1
        then error"ошибка, числа a и m должны быть взаимно просты"
        else v `mod` m

    where (v, u, d) = nodExtend a b


funcEuler :: Int -> Int
funcEuler m = length $ filter f [2 .. m-1]
    where f = (\i -> m `nod` i == 1)


lastDigit Int -> Int -> Int -> Int
lastDigit a b c = powMod a x 10
    where x = powMod b c $ funcEuler 10


-- 4 задача --


polinomeMod [] _ _= 0
polinomeMod (a:as) x m = a + n_1 `mod` m
  where n_1Nome = polinome as x
        n_1 = x * n_1Nome `mod` m
