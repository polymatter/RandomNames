import System.Random

type Name   = (String,Int)
type Rand a = StdGen -> (a, StdGen)

data Weight = Low | Std | High
     deriving (Show,Eq,Ord)

---------

weight :: Weight -> Int
weight Low  = 10
weight Std  = 20
weight High = 30

consts :: [(Char, Weight)]
--consts = zip "sfhjv#" [High, Std, Low, Low, Std, High] 
consts = zip "qwrtypsdfghjklzxcvbnm" (repeat Std)

vowels :: [(Char, Weight)]
vowels = zip "aeiou" [High, Std, Low, Std, High]

-----------

roots_cv :: [(String,Int)]
roots_cv = do (c,w1) <- consts
              (v,w2) <- vowels
              return ([c,v],weight w1 + weight w2)

roots_cvc :: [(String,Int)]
roots_cvc = do (c1,w1) <- consts
               (v ,w2) <- vowels
               (c2,w3) <- consts
               return ([c1,v,c2],weight w1 + weight w2 + weight w3)

tarkin :: [(a,Int)] -> [(a,Int)]
tarkin xs = zip (map fst xs) (commulative (map snd xs)) 

-- commulative [1..5]
-- [1,3,6,10,15]
commulative :: [Int] -> [Int]
commulative = commulative' 0

commulative' :: Int -> [Int] -> [Int]
commulative' a []     = []
commulative' a (x:xs) = (a+x) : (commulative' (a+x) xs)

------------

namebetween :: [(String,Int)] -> Int -> String
namebetween []          _ = "error"
namebetween ((n,w):nws) r  
   | w > r     = n
   | otherwise = namebetween nws (r-w)

plobbin :: StdGen -> [(String, Int)] -> (String, StdGen)
plobbin g ns = let (r_min, r_max)  = genRange g
                   (r_raw   , g')  = next g
                   -- r_norm ::Int between 0 and 100  
                   r_norm           = (r_raw - r_min) `div` (r_max `div` 10000)
                   w_sum           = sum $ map snd ns
                   -- r ::Int between 0 and w_sum
                   r               = (r_norm * w_sum) `div` 10000
               in (namebetween ns r, g')

----


name_cvcvcv :: StdGen -> (String, StdGen)
name_cvcvcv g0 = let (cv1,g1) = plobbin g0 roots_cv
                     (cv2,g2) = plobbin g1 roots_cv
                     (cv3,g3) = plobbin g2 roots_cv
                 in (cv1 ++ cv2 ++ cv3, g3)

name_cvccv :: StdGen -> (String, StdGen)
name_cvccv g0 = let (cvc,g1) = plobbin g0 roots_cvc
                    (cv ,g2) = plobbin g1 roots_cv
                in (cvc ++ cv, g2)

---------

pre_roots :: [(String, Int)]
pre_roots = roots_cv ++ roots_cvc

names :: StdGen -> [StdGen -> (String, StdGen)] -> [String]
names _  []     = []
names g0 [f]    = let (s, _ ) = f g0 in [s]
names g0 (f:fs) = let (s, g1) = f g0
                  in s:(names g1 fs)

------------

main = do g <- newStdGen
          sequence $ map print (names g [name_cvcvcv,name_cvccv])
          return ()