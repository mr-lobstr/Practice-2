import System.Random
import Data.Set (Set, fromList, toList)

rand :: IO Int
rand = randomIO


randUniqueNums :: Integer -> Integer -> Int -> Int -> [Integer]
randUniqueNums a b n g = take n rndL
    where rndL = randomRs (a, b) $ mkStdGen g


type Key = (Integer, Integer)


polinomeMod [] _ _ = 0
polinomeMod (a:as) m x = (a + n_1) `mod` m

  where n_1Nome = polinomeMod as m x
        n_1 = x * n_1Nome `mod` m


powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod a power n = if even power
                   then result
                   else result * a `mod` n

    where newA = a * a `mod` n
          newPower = power `div` 2
          result = powMod newA newPower n


keysGen :: Integer -> [Integer] -> [Integer] -> Integer -> [Key]
keysGen secret xs coefs prime = zip xs ys
    where ys = map computeX xs
          computeX = polinomeMod coefs prime


monomeLagrange :: Integer -> [Integer] -> Integer
monomeLagrange p (yJ:xJ:xs) = (num * denom) `mod` p

    where numF = (\n x -> -x * n `mod ` p)
          denomF = (\d x -> d * powMod (xJ - x) (p - 2) p)
          num = foldl numF yJ xs
          denom = foldl denomF 1 xs


secretRecovery :: [Key] -> Integer -> Integer
secretRecovery keys prime = foldl secretF 0 combXY

    where xs = map fst keys
          combF = (\(x, y) -> y : x : filter (/= x) xs)
          combXY = map combF keys
          mono = monomeLagrange prime
          secretF = (\s xy -> (s + mono xy) `mod` prime)

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) &&
                        (not (value1 && value2))


xorPair :: (Bool,Bool) -> Bool
xorPair (v1,v2) = xorBool v1 v2


xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)


type Bits = [Bool]


intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
        if (remainder == 0)
        then False : intToBits' nextVal
        else True : intToBits' nextVal

    where remainder = n `mod` 2
          nextVal = n `div` 2


maxBits :: Int
maxBits = length (intToBits' maxBound)


intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (intToBits' n)
          missingBits = maxBits - (length reversedBits)
          leadingFalses = take missingBits (cycle [False])


charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)


bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLoc)
    where size = length bits
          indices = [size-1,size-2 .. 0]
          trueLoc = filter (\x -> fst x == True) (zip bits indices)


bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair -> (fst pair) `xor` (snd pair))
                                (zip padBits plaintextBits)

    where padBits = map charToBits pad
          plaintextBits = map charToBits plaintext


applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
    where bitList = applyOTP' pad plaintext


prng :: Int -> Int -> Int -> Int -> String
prng a b maxNumber seed = chr : prng a b maxNumber result
    where result = (a*seed + b) `mod` maxNumber
          chr = bitsToChar $ intToBits result


main :: IO ()
main = do
    g <- rand
    g2 <- rand
    sct <- rand

    let secret = 42
    let prime = 73
    let n = 4
    let k = 3

    let xs = randUniqueNums 1 (2 ^ 64) n g
    let coefs = secret : randUniqueNums 0 prime (k-1) g2
    let keys = keysGen 42 xs [42, 1, 2] prime

    let recSec = secretRecovery (drop 1 keys) prime

    let s = "qwerty раз два три четыре"
    let s3 = prng 123 456 (2^20) 42
    let s2 = applyOTP s s3

    putStrLn $ applyOTP s2 s3