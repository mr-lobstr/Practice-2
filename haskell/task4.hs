import System.IO
import System.Random
import Data.Set (fromList, toList)

powMod :: (Integral a) => a -> a -> a -> a
powMod _ 0 _ = 1
powMod a power n = if even power
                   then result
                   else result * a `mod` n

    where newA = a * a `mod` n
          newPower = power `div` 2
          result = powMod newA newPower n


class (Integral a) => FModulo a where
    modulo :: a

    toFModulo :: a -> a
    toFModulo a = a `mod` modulo
    
    addM :: a -> a -> a
    addM a b = toFModulo (toFModulo a + toFModulo b)

    subM :: a -> a -> a
    subM a b = toFModulo (toFModulo a - toFModulo b)

    mulM :: a -> a -> a
    mulM a b = toFModulo (toFModulo a * toFModulo b)

    powM :: a -> a -> a
    powM _ 0 = 1
    powM a n = if even n
               then result
               else result `mulM` a

        where newN = n `div` 2
              result = (a `mulM` a) `powM` newN

    divM :: a -> a -> a
    divM a b = a `mulM` invB
        where invB = b `powM` (modulo-2)


instance FModulo Int where
    modulo = 3701


polinomeM :: [Int] -> Int -> Int
polinomeM [] _ = 0
polinomeM (c:coefs) x = c `addM` n_1Nome
  where n_1Nome = x `mulM` polinomeM coefs x


type Key = (Int, Int)

keysGen' :: [Int] -> [Int] -> [Key]
keysGen' coefs xs = zip xs ys
    where ys = map (polinomeM coefs) xs


keysGen :: Int -> Int -> Int -> StdGen -> [Key]
keysGen secret n k gen = keysGen' coefs xs
    
    where coefs' = randUniqueNums (k-1) gen
          coefs = secret : map toFModulo coefs'
          xs = randUniqueNums n $ snd $ next gen


monomeLagrange :: Int -> Int -> [Int] -> Int
monomeLagrange yJ xJ xs' = num `mulM` denom

    where xs = filter (/= xJ) xs'
          num = foldl mulM yJ $ map negate xs
          denom = foldl divM 1 $ map (xJ -) xs


secretRecovery :: [Key] -> Int
secretRecovery keys = foldl addM 0 monomes

    where xs = map fst keys
          monomeF = (\(x, y) -> monomeLagrange y x xs)
          monomes = map monomeF keys


rand :: IO Int
rand = randomIO

randUniqueNums :: Int -> StdGen -> [Int]
randUniqueNums n gen = take n newL
    where rndL = take (n*10) $ (randoms gen :: [Int])
          newL = toList $ fromList rndL


data Bit = B0 | B1 deriving (Eq, Ord, Enum)
type Bits = [Bit]

intToBits' :: Int -> Bits
intToBits' 0 = [B0]
intToBits' 1 = [B1]
intToBits' n = lastBit : intToBits' (n `div` 2)
    where lastBit = if even n then B0 else B1


maxBits :: Int
maxBits = length $ intToBits' $ fromEnum (maxBound :: Char)


intToBits :: Int -> Bits
intToBits n = remBits ++ bits
    where bits = reverse (intToBits' n)
          lenRem = maxBits - (length bits)
          remBits = take lenRem $ cycle [B0]


charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)


bitsToInt :: Bits -> Int
bitsToInt bits = sum $ zipWith powF bits pows
    where size = length bits
          pows = reverse $ take size [0 ..]
          powF = (\b p -> if b == B1 then 2 ^ p else 0)


bitsToChar :: Bits -> Char
bitsToChar bits = toEnum $ bitsToInt bits


xor :: Bits -> Bits -> Bits
xor b1 b2 = zipWith xorBit b1 b2
    where xorBit = (\b1 b2 -> if b1 /= b2 then B1 else B0)


pseudoRndSymbol' :: Int -> Int -> Int -> (Bits, Int)
pseudoRndSymbol' 0 g _ = ([], g)
pseudoRndSymbol' n x_n m = (bit : bits, g)
    where bbsR = powMod x_n 2 m
          bit = if even bbsR then B0 else B1
          (bits, g) = pseudoRndSymbol' (n-1) bbsR m


pseudoRndSymbol :: Int -> Int -> (Char, Int)
pseudoRndSymbol x_n m = (bitsToChar bits, g)
    where (bits, g) = pseudoRndSymbol' (maxBits - 1) x_n m


pseudoRndStr :: Int -> Int -> String
pseudoRndStr x_n m = chr : str
    where (chr, g) = pseudoRndSymbol x_n m
          str = pseudoRndStr g m


enAndDecode' :: String -> String -> [Bits]
enAndDecode' pad text = zipWith xor padB textB
    where padB = map charToBits pad
          textB = map charToBits text


enAndDecode :: String -> String -> String
enAndDecode pad text = map bitsToChar bits
    where bits = enAndDecode' pad text


main :: IO ()
main = do
    let p = 6025631
    let q = 8478859
    let n = 10
    let k = 3
    let secret = 1141

    rnd <- rand
    let gen = mkStdGen rnd
    let keys = keysGen secret n k gen

    text <- readFile "test.txt"
    let rndStr = pseudoRndStr secret (p * q)
    let enText = enAndDecode rndStr text

    -- writeFile "encode.txt" enText
    -- putStrLn enText
    
    let secretRec = secretRecovery $ take k keys
    let rndStr2 = pseudoRndStr secretRec (p * q)
    let deText = enAndDecode rndStr enText

    writeFile "decode.txt" deText

    putStrLn $ show $ length enText