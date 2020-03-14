--Lambda Calculus
-- 1a (\x.x y) z
--      z y
-- 1b (\x.y) z
--       y
-- 1c \x.\y.(\z.z x y) a b c
--     \y.(\z.z a y) b c
--     (\z. z a b) c
--       c a b
-- 1d. (\x.\y.x y)(\y.y y)
--      \y.(\y.y y) y
--      (\y.y y)
-- 1e. (\x.x x)(\x.x x)
--      (\x.x x) x
--          x x
-- 1f. (\x.\y.x) z
--        \y.z
-- 2. multi = \m.\n. m plus(z plus(n n))
--    even = \n.n not true



--4.Harmomnic
harmonic :: Int -> Double

harmonic 1 = 1
harmonic n = 1 / fromIntegral(n) +  harmonic (n-1)

--5.CountQs

countQs :: String -> Int

countQs[] = 0
countQs('q':t) = 1 + countQs(t)
countQs('Q':t) = 1 + countQs(t)
countQs(_:t) = countQs(t)

--6.mytake

mytake :: Int -> [String] -> [String]

mytake 1 s = [head s]
mytake n [] = []
mytake 0 s = []
mytake n (h:t) = [h] ++ mytake (n-1) t


--7.mylast

mylast :: [Int] -> Int

mylast [] = 0
mylast [x] = x
mylast(_:t) = mylast t


--8.range
range :: Int -> Int -> [Int]

range a b
   |a > b = []
   |a == b = [a]
   |b > a = [a] ++ range (a+1) b


--9.longestRun
--longestRun :: String -> Int
{-
run :: Char -> String -> Int
run s [t:ts] =
    if t == s
    then 1 + run s ts
    else run s ts
-}

--helper :: Int -> Char -> String -> Int
helper count head (h:t)
    | head == [] = 0
    | head /= h = count
    | head == h = (helper (count+1) (head) (t))

helloWorld :: String -> String

helloWorld s = "hello world"