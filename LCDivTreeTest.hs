module LCDivTreeTest (associative, left_commutative,  prop_cancel, randFrac) where
import List
import General
import LCDivTree
import Random

associative :: (RFraction,RFraction,RFraction) -> Bool
associative (a,b,c) = a %* ( b %* c) == ( a %* b ) %* c 
-- genAssoc :: [RFraction] -> Bool
-- genAssoc [] = True
-- genAssoc [a] = True
-- genAssoc [a,b] = True
-- genAssoc s =
--     all (map [1 .. (length s)])

left_commutative :: (RFraction,RFraction,RFraction) -> Bool
left_commutative (a,b,c) = a %+ ( b %+ c ) == b %+ ( a %+ c ) 

prop_cancel :: (RFraction,RFraction) -> Bool
prop_cancel (a,b) = (cancel (umul (cancel a) (cancel b))) == (cancel (umul a b))
prop_ncancel :: (Int,RFraction,RFraction) -> Bool
prop_ncancel (n,a,b) = (cn n (umul (cn n a) (cn n b))) == (cn n (umul a b))

assoc :: ( RFraction -> RFraction -> RFraction ) -> (RFraction,RFraction,RFraction) -> IO Bool
assoc m (r,s,t) = 
    let 
    rp = r `m` ( s `m` t )
    lp = ( r `m` s ) `m` t
    res = lp == rp 
    in
    do
    if (not res)
       then
       do
       putStrLn ("  (rs)t = " ++ (show lp))
       putStrLn ("  r(st) = " ++ (show rp))
       else
       putStrLn "ok"
    return res

leftcomm :: ( RFraction -> RFraction -> RFraction ) -> (RFraction,RFraction,RFraction) -> IO Bool
leftcomm a (r,s,t) = return (r `a` ( s `a` t ) == s `a` ( r `a` t ))

canc :: ( RFraction -> RFraction ) -> (RFraction,RFraction) -> IO Bool
canc c (r,s) = 
    let 
    cr = c r
    cs = c s
    crs = c ( r `umul` s)
    ccrcs = c ( cr `umul` cs )
    res = ccrcs == crs
    in
    do
    if (not res)
       then
       do
       putStrLn ("  r=           " ++ (show r))
       putStrLn ("  s=           " ++ (show s))
       putStrLn ("  rs=          " ++ (show (r `umul` s)))
       putStrLn ("  c(r)=        " ++ (show cr))
       putStrLn ("  c(s)=        " ++ (show cs))
       putStrLn ("  c(r)*c(s)=   " ++ (show (cr `umul` cs)))
       putStrLn ("  c(r*s)=      " ++ (show crs))
       putStrLn ("  c(c(r)*c(s))=" ++ (show ccrcs))
       else
       putStr ""
    return res

rFractionList :: Int -> Int -> [ RFraction ]
rFractionList d b | d <= 0 = [e]
                  | d > 0  = 
                      let a = kMaxBags b (rFractionList (d-1) b)
                      in
                      [ Frac den nom | den <- a, nom <- a ]

rFractionList2 d b | d <= 0 = [e]
                   | d > 0  = 
                       let l = kSets 2 (kMaxBags b (rFractionList2 (d-1) b))
                       in
                       e : 
                             [ (Frac a b)  | [a,b] <- l ] ++ 
                             [ (Frac b a)  | [a,b] <- l ]

-- randChoose 3 2 yields a value of [1,2], [1,3] or [2,3]
randChoose :: Int -> Int -> IO [Int]
randChoose n k = 
    if ( (k<0) || (k>n) ) then (return []) else
    let 
    choose arg = 
        do
        (a,b) <- arg
        i <- getStdRandom (randomR (0,(length a)-1))
        return ((deli i a),((a !! i):b))
    in
    do
    (_,c) <- (iterate choose (return ([1 .. n],[]))) !! k
    return (sort c)

randCFrac :: Int -> IO RFraction
randCFrac n = do
             t <- randFrac n
             return (c t)

randFrac :: Int -> IO RFraction
randFrac n = 
    let
    randomBreadth n = 
        let
        x :: Double
        x = (log (fromIntegral n))
        in
        do
        logk <- getStdRandom (randomR (0,x))
        return (round (exp logk))
--     randomBreadth m = fst(randomR (1,m) g)

    diffs prev [] = []
    diffs prev (f:r) = (f - prev) : (diffs f r)
    chooseSum m = 
        if m == 0 then return [] else do
           breadth <- (randomBreadth m)
           a <- (randChoose (m-1) (breadth-1)) 
           return (diffs 0 (a++[m]))
    -- sum chooseSum  == n, chooseSum !! i > 0
    in
    do
    luSplit <- (getStdRandom (randomR (0,n-1)))

    lowN <- return luSplit
    uppN <- return (n - 1 - luSplit)
    -- lowN, uppN >= 0, lowN + uppN + 1 == n

    ln <- (chooseSum lowN)
    un <- (chooseSum uppN)
    lr <- sequence (map randFrac ln)
    ur <- sequence (map randFrac un)
    return (Frac lr ur)

rep :: Int -> IO Bool -> IO Bool
rep n t = 
    do
    res <- sequence (replicate n t)
    print (and res)
    return (and res)


rt3 :: ( (RFraction,RFraction,RFraction) -> IO Bool ) -> Int -> IO Bool
-- t is test, nc nodesCount
rt3 test nodeCount =
    do
    r1 <- randCFrac nodeCount
    r2 <- randCFrac nodeCount
    r3 <- randCFrac nodeCount
    putStrLn ("" ++ (show r1) ++ " " ++  (show r2) ++ " " ++ (show r3))
    res <- test (r1,r2,r3)
    if ( not res ) 
       then putStrLn " -> FAILED" 
       else putStr ""
    return res 

-- example
testAssoc = rep 100 (rt3 (assoc oc) 8)

rt3max :: ( (RFraction,RFraction,RFraction) -> IO Bool ) -> Int -> IO Bool
-- t is test, nc nodesCount
rt3max test nodeCount =
    do
    n1 <- getStdRandom (randomR (1,nodeCount))
    n2 <- getStdRandom (randomR (1,nodeCount))
    n3 <- getStdRandom (randomR (1,nodeCount))
    r1 <- randFrac n1
    r2 <- randFrac n2
    r3 <- randFrac n3
    putStrLn ("" ++ (show r1) ++ " " ++  (show r2) ++ " " ++ (show r3))
    res <- test (r1,r2,r3)
    if ( not res ) 
       then putStrLn " -> FAILED" 
       else putStr ""
    return res 

rt2 :: ( (RFraction,RFraction) -> IO Bool ) -> Int -> IO Bool
rt2 test nodeCount =
    do
    r1 <- randFrac nodeCount
    r2 <- randFrac nodeCount
    putStrLn ("" ++ (show r1) ++ " " ++  (show r2) )
    res <- test (r1,r2)
    if ( not res ) 
       then putStrLn " -> FAILED" 
       else putStr ""
    return res
    
rt2max :: ( (RFraction,RFraction) -> IO Bool ) -> Int -> IO Bool
rt2max test nodeCount =
    do
    n1 <- getStdRandom (randomR (1,nodeCount))
    n2 <- getStdRandom (randomR (1,nodeCount))
    r1 <- randFrac n1
    r2 <- randFrac n2
    putStrLn ("" ++ (show r1) ++ " " ++  (show r2) )
    res <- test (r1,r2)
    if ( not res ) 
       then putStrLn " -> FAILED" 
       else putStr ""
    return res

randomTest_cancel :: Int -> IO Bool
randomTest_cancel nodesCount =
    do
    nr <- getStdRandom (randomR (1,nodesCount))
    ns <- getStdRandom (randomR (1,nodesCount))
    r <- randFrac nodesCount
    s <- randFrac nodesCount
    print r
    print s
    print (r `umul` s)
    b <- return (prop_cancel (r,s))
    print b
    return b

randomTest_ncancel :: Int -> Int -> IO Bool
randomTest_ncancel nodesCount depth =
    do
    nr <- getStdRandom (randomR (1,nodesCount))
    ns <- getStdRandom (randomR (1,nodesCount))
    r <- randFrac nr
    s <- randFrac ns
    print r
    print s
    print (r `umul` s)
    res <- return (prop_ncancel (depth,r,s))
    print res
    return res

remove_aia :: RFraction -> RFraction
remove_aia (Frac a b) =
    let a' = [remove_aia ae | ae <- a ]
        b' = [remove_aia be | be <- b ]
    in
    if (existBij (==) a' b') then e
    else Frac a' b'
