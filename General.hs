module General (deli, indexes, existBij, existBijI, prod, proj, x, p1, p2, xx, pp1, pp2, pp3, firstMatchI, firstMatch, bagMinus, bagCut, bagsCut, removeMatch, kTupels, kBags, kSets, kMaxTuples, kMaxBags, kMaxSets, bagEq, subBag, powerSet, powerBag) where
import List

kTupels :: Int -> [t] -> [[t]]
kTupels k s = prod (replicate k s)

kBags :: Int -> [t] -> [[t]]
kBags k [] | k <= 0 = [[]]
           | k > 0  = []
kBags k (se:sr) | k <= 0 = [[]]
                | k > 0  = concat [ [ (replicate i se) ++ y | y <- kBags (k-i) sr ] | i <- [ 1 .. k ] ] ++ (kBags k sr)

kSets :: Int -> [t] -> [[t]]
kSets k [] | k <= 0 = [[]]
           | k > 0  = []
kSets k (se:sr) | k <= 0 = [[]]
                | k > 0  = [ se:y | y <- kSets (k-1) sr ] ++ (kSets k sr)

kMaxTuples k s = concat [ kTupels i s | i <- [0..k] ]
kMaxBags k s = concat [ kBags i s | i <- [0..k] ]
kMaxSets k s = concat [ kSets i s | i <- [0..k] ]
listsOfSet s = kMaxTuples (length s) s

prod :: [[t]] -> [[t]]
prod [] = [[]]
prod ([]:_) = []
prod ((a:ar):r) = (map (a:) (prod r)) ++ (prod (ar:r))

proj :: Int -> [[t]] -> [t]
proj i s = map (\se -> se!!i) s

x :: [t] -> [t] -> [(t,t)]
x s1 s2 = map (\(e1:e2:r) -> (e1,e2)) (prod [s1,s2])

p1 = map (\(e1,e2) -> e1)
p2 = map (\(e1,e2) -> e2)

xx :: [t] -> [t] -> [t] -> [(t,t,t)]
xx s1 s2 s3 = map (\(e1:e2:e3:r) -> (e1,e2,e3)) (prod [s1,s2,s3])

pp1 = map (\(e1,e2,e3) -> e1 )
pp2 = map (\(e1,e2,e3) -> e2 )
pp3 = map (\(e1,e2,e3) -> e3 )


bagChar1 :: Eq a => ([(a,Int)],[a]) -> ([(a,Int)],[a])
bagChar1 (s,[]) = (s,[])
bagChar1 (s,(t':tr)) =
    case findIndex (\(s',_) -> s' == t' ) s of
    Nothing -> bagChar1((s ++ [(t',1)]),tr)
    Just i -> let (_,c) = s!!i 
              in bagChar1((take i s) ++ ((t',c+1) : (drop (i+1) s)),tr)

bagChar :: Eq a => [a] -> [(a,Int)]
bagChar s = let (res,_) = bagChar1 ([],s) in res

bagSeq :: [(a,Int)] -> [a]
bagSeq [] = []
bagSeq ((s',k):sr) = (replicate k s') ++ (bagSeq sr)
    

-- powerBagI :: [Int] -> [[Int]]
-- powerBagI s =
--     s : concat (map (\i -> map concat (prod [(powerBag (take i s)),[ (drop (i+1) s)]]) ) (indexes s))
powerBag1 :: Eq a => [(a,Int)] -> [[(a,Int)]]
powerBag1 [] = [[]]
powerBag1 ((s',k):sr) = 
    concat (map (\i -> [(s',i):t | t <- powerBag1 sr]) [0..k])

powerBag :: Eq a => [a] -> [[a]]
powerBag s = map bagSeq ( powerBag1 ( bagChar s))
    
-- regard [a] as set (i.e. only different elements) returns the powerset
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (s':sr) = [s':t | t <- powerSet sr] ++ (powerSet sr)

indexes :: [t] -> [Int]
indexes a = [0..(length a)-1]

existInjI :: (Int -> Int -> Bool) -> [Int] -> [Int] -> Bool
existInjI p [] y = True
existInjI p (x1:xr) y =
    any (\y1 -> (p x1 y1) && (existInj p xr (delete y1 y))) y

existInj :: (a -> b -> Bool) -> [a] -> [b] -> Bool
existInj p s t = existInjI (\si ti -> p (s!!si) (t!!ti)) (indexes s) (indexes t)

subBag :: Eq a => [a] -> [a] -> Bool
s `subBag` t = existInj (==) s t

existBijI :: (Int -> Int -> Bool) -> [Int] -> [Int] -> Bool
existBijI p [] y = y == []
existBijI p (x1:xr) y = 
    any (\y1 -> (p x1 y1) && (existBij p xr (delete y1 y))) y

existBij :: (a -> b -> Bool) -> [a] -> [b] -> Bool
existBij p s t = existBijI (\si ti -> p (s!!si) (t!!ti)) (indexes s) (indexes t)

bagEq :: Eq a => [a] -> [a] -> Bool
s `bagEq` t = existBij (==) s t 

-- maybe equal to s `intersect` t
bagCut :: Eq a => [a] -> [a] -> [a]
s `bagCut` t = p1 (firstMatch (==) (s,t))

bagsCut :: Eq t => [[t]] -> [t]
bagsCut [a] = a
bagsCut (a:ar) = a `bagCut` (bagsCut ar)

firstMatchI :: (Int -> Int -> Bool) -> ([Int],[Int]) -> [(Int,Int)]
firstMatchI _ ([],_) = []
firstMatchI _ (_,[]) = []
firstMatchI p (s,t) =
    case find (\(si,ti) -> p si ti) (x s t) 
    of
    Nothing -> [] 
    Just (si,ti) -> (si,ti) : firstMatchI p (delete si s,delete ti t)

firstMatch :: (a -> b -> Bool) -> ([a],[b]) -> [(a,b)]
firstMatch p (s,t) = 
    map (\(si,ti) -> (s!!si,t!!ti))
            (firstMatchI (\si ti -> p (s!!si) (t!!ti)) (indexes s,indexes t))

deli :: Int -> [a] -> [a]
deli k s = take k s ++ drop (k+1) s

bagMinus :: Eq a => [a] -> [a] -> [a]
s `bagMinus` t =
    case find (\(ti,si) -> (s!!si) == (t!!ti)) (x (indexes t) (indexes s)) of
    Nothing -> s
    Just (ti,si) -> (deli si s) `bagMinus` (drop (ti+1) t)

removeMatch :: (a -> b -> Bool) -> ([a],[b]) -> ([a],[b])
removeMatch p (s,t) =
    let sI = indexes s
        tI = indexes t
        match = firstMatchI (\si ti -> p (s!!si) (t!!ti)) (sI,tI)
        ps = sI `bagMinus` (p1 match) 
        pt = tI `bagMinus` (p2 match) 
    in
    (map (\si -> s!!si) ps, map (\ti -> t!!ti) pt)
