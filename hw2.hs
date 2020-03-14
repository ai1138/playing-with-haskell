import Data.Char (toUpper)
{- Lambda Calculus
  1. pred 2
  (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) (λf.λx.f (f x))




  2.sum = Y (λf.λn.if (iszero n) 0 (plus n (f (pred n))))
-}

--stutterN

stutterN :: Int -> [a] -> [a]

stutterN 0 [] = []
stutterN n [] = []
stutterN 1 arr = arr
stutterN n (h:t) = repeatN n h ++ stutterN (n) t


repeatN :: Int -> a -> [a]
repeatN 0 a = []
repeatN n a = [a] ++ repeatN (n-1) a

--onlyVowelsToUppercase
onlyVowelsToUppercase :: String -> String
isVowel x = x `elem` ['A','E','I','O','U']
onlyVowelsToUppercase s = filter isVowel (map toUpper s)

--length2

length2 :: [a] -> Int

length2 a = foldl (\x _ -> x+1 ) 0 a

--uniq
uniq :: [Char] -> [Char]

uniq [] = []
uniq (h:t) = h:uniq (filter ((/=) h) t)

--mostOdds
mostOdds :: [[Int]] -> [Int]
{-}
