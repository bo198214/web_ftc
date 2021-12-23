module LCDivTree 
    (
     rf1, rf2, rf3, rf4, rf5, rf6, rf, 
     RFraction(Frac), RSemiFraction, e, inc, inv, (%*), (%+),
     umul, hmul, oc, cancel, c, cn, cn2, ncancel,
     fractionDepth1, fractionDepth, fractionCount1, fractionCount,
     toTotalDivTree1, toTotalDivTree, showAsLatex, showAsLatextf, showAsSimple, showAsString1, show,
    ) where
import General
import List
import CoppiceInc
import qualified TotalDivTree


-- recursive Fractions
data RFraction = Frac { den :: [RFraction] , nom :: [RFraction] }
type RSemiFraction = [RFraction]

instance Eq RFraction where
    (Frac al au) == (Frac bl bu) = 
        (existBij (==) au bu) &&
        (existBij (==) al bl)

subFrac :: RFraction -> RFraction -> Bool
r `subFrac` s = ((den r) `subBag` (den s)) && ((nom r) `subBag` (nom s))

instance CoppiceInc RFraction where
    e = Frac [] []
    r %* s = r `oc` s
    inv (Frac tl tu) = Frac tu tl
    inc t = Frac [] [t]

frac :: (RSemiFraction,RSemiFraction) -> RFraction
frac (a,b) = Frac a b

oc :: RFraction -> RFraction -> RFraction
r `oc` s = 
    let 
    (ls,ur) = removeMatch (==) (den s,nom r) 
    h = Frac ([ lis `oc` (inv r) | lis <- ls ] ++ (den r))
             ([ uir `oc` s | uir <- ur ] ++ (nom s))
    -- pairs of (den r)`(nom s) together with their distance
    triples = [ (lir,uis,(inv lir) `oc` uis) | 
                lir <- (nub (den r)), uis <- (nub (nom s)) ]
    -- cancel canditates
    ccs = [ (lir,uis,u) | (lir,uis,u) <- triples, u `subFrac` h ]
    -- u values
    us = nub [ u | (lir,uis,u) <- ccs ]
    -- h \ u
    hwo u = removeMatch (\x' y' -> (x',y',u) `elem` ccs) ((den h),(nom h))
    in
    case find (\u -> u == frac(hwo u) ) us of
    Nothing -> h
    Just u -> u

c :: RFraction -> RFraction 
c t = (Frac [c(ti) | ti <- (den t)] []) `oc` (Frac [] [c(ti)| ti <- (nom t)])


-- non core functionalities

umul :: RFraction -> RFraction -> RFraction
umul a b =
    Frac ((map (\bl -> umul bl (inv a)) (den b)) ++ (den a))
         ((map (\au -> umul au       b) (nom a)) ++ (nom b))

uprod :: [RFraction] -> RFraction
uprod [] = e
uprod (a : r) = umul a (uprod r)

-- hmul not associative for example (r,s,t) = [2\] 2 [1,1]\
--   (rs)t = [[2\2,1]\]\[[2\2,1]\]
--   r(st) = 1

hmul :: RFraction -> RFraction -> RFraction
hmul a b =
    let (denb',noma') = removeMatch (==) (den b,nom a) 
    in
    Frac ((map (\bl -> hmul bl (inv a)) denb') ++ (den a))
         ((map (\au -> hmul au       b) noma') ++ (nom b))

cmul :: RFraction -> RFraction -> RFraction
cmul a b = ncancel (max (fractionDepth a) (fractionDepth b)) (hmul a b)
    
-- udistant ::  RFraction -> RFraction -> Bool
-- udistant t u = existBij (\x y -> (inv x) `oc` y == u) (den t) (nom t)

newtype RPrimeRep = PR { prs :: [ (Bool,[RPrimeRep]) ] } 
instance Eq RPrimeRep where
    (PR []) == (PR []) = True
    (PR []) == (PR (_:_)) = False
    (PR (_:_)) == (PR []) = False
    (PR ((ss,se):sr)) == (PR ((ts,te):tr)) =
        (ss == ts) && (se `bagEq` te)

instance Show RPrimeRep where
    show (PR []) = "1"
    show (PR [e]) = (showLetter e)
    show (PR (e:r)) = (showLetter e) ++ (show (PR r))

showLetterMid :: [ RPrimeRep ] -> String
showLetterMid [] = ""
showLetterMid [a] = show a
showLetterMid s = (showLetterMid (init s)) ++ "," ++ (show (last s))

showLetter :: (Bool,[RPrimeRep]) -> String
showLetter (True,[PR []]) = "2"
showLetter (False,[PR []]) = "2~"
showLetter (True,[PR [(True,[PR []])]]) = "3"
showLetter (False,[PR [(True,[PR []])]]) = "3~"
showLetter (True,s) = "[" ++ (showLetterMid s) ++ "]"
showLetter (False,s) = "[" ++ (showLetterMid s) ++ "]~"

instance CoppiceInc RPrimeRep where
    e = PR []
    inv (PR s) = PR [ ((not ss),se) | (ss,se) <- (reverse s) ]
    inc pr = PR [ (True,[pr]) ]
    (PR a) %* (PR b) = PR (a ++ b)
--     a %* (PR []) = a
--     (PR []) %* b = b
--     (PR a) %* (PR b) = 
--         let (asig,a') = last a
--             (bsig,b') = head b
--         in
--         if (a' == b') && (asig /= bsig) then (PR (init a)) %* (PR (tail b))
--         else PR (a ++ b)

primeSemiRep1 :: [ RPrimeRep ] -> RPrimeRep
primeSemiRep1 [] = e
primeSemiRep1 s  =
   let equs = groupBy ( \(PR a) (PR b) -> 
                        case (a,b) of
                        ([],[]) -> True
                        ([],_) -> False
                        (_,[]) -> False
                        (a,b) -> (last a) == (last b)
                      ) s
       lasteqel :: [RPrimeRep] -> (Bool,[RPrimeRep])
       lasteqel ((PR ce):_) = case ce of
                              [] -> (True,[])
                              _ -> last ce
       lasteqseq c = let (_,seq) = lasteqel c in seq
   in
   case find (\c -> 
                  let (sign,seq) = lasteqel c
                  in
                  (seq /= []) && (sign == True ) && 
                  (s `bagEq` (c ++ (lasteqseq c)))
             ) equs of
   Nothing -> PR [(True,s)]
   Just c -> (primeSemiRep1 (map (\(PR c') -> PR (init c')) c)) %* (PR [lasteqel c])

primeSemiRep :: RFraction -> RPrimeRep
primeSemiRep (Frac s t) = 
        (inv (primeSemiRep1 [ primeSemiRep se | se <- s ])) %*
        (     primeSemiRep1 [ primeSemiRep te | te <- t ])

primeRep1 :: ([RPrimeRep],[RPrimeRep]) -> RPrimeRep
-- s\t = (a\b) (True,p), s\t = (a\b) (False,q)
primeRep1 ([],[]) = PR []
primeRep1 (s,t) =
    let (te,tne) = partition (\t' -> t' == e) t
        val = map (\(_,x) -> x)
        (pps,qms) = (partition (\(sign,_) -> sign) [last t' | (PR t') <- tne])
        bOf = (\pqpm -> [ PR (init t') | (PR t') <- tne, (last t') == pqpm])
        prime = case (s,t) of 
                       ([],_) -> PR [(True,t)]
                       (_,[]) -> PR [(False,s)]
                       (_,_) -> PR [(False,s),(True,t)]
    in 
    case find (\(True,p) -> 
                   ([ PR t' |  (PR t') <- tne, (last t') /= (True,p)] ++ te) 
                   `bagEq` p
              ) pps 
    of Just pp -> (primeRep1(s,bOf pp)) %* (PR [pp])
       Nothing -> 
           if (t == []) then inv (primeRep1([],s))
           else if (qms == []) then prime
           else if (s == []) then prime
           else
           let (qm:_) = qms
               b = bOf qm
               (False,q) = qm
           in 
           if (te == []) && (all (\(PR t') -> (last t') == qm ) tne) then
              case find (\a -> 
                         let PR iba = primeRep1 (b,a) in 
                         ([ PR (q' ++ iba) | PR q' <- q ] ++ a) `bagEq` s
                        ) (powerBag s) of
              Nothing -> prime
              Just a -> (primeRep1(a,b)) %* (PR [qm])
           else prime


primeRep :: RFraction -> RPrimeRep
primeRep (Frac s t) =
    primeRep1 ( [ primeRep s' | s' <- s ], [ primeRep t' | t' <- t ] )

fracFromPair (s,t) = Frac s t
pairFromFrac (Frac s t) = (s,t)

ncancel :: Int -> RFraction -> RFraction
ncancel n (Frac a b) | n <= 0 = (Frac a b)
                     | n > 0 =
    let a' = map (ncancel n) a
        b' = map (ncancel n) b
    in
    fracFromPair (removeMatch (\x y -> (ncancel (n-1)) (hmul (Frac [] (delete x a')) (inv x)) == (ncancel (n-1)) (hmul (Frac [] (delete y b')) (inv y))) (a',b'))
-- ncancel is not always associative:
-- ncancel 1 (umul (ncancel 1 (umul rf3 (inv rf2))) (umul (umul rf2 rf3) (inv rf3)) ) =! 3
-- but ncancel 1 [a_i]\[a_i] = 1


nucancel :: Int -> RFraction -> RFraction
-- ncancel with umul instead of hmul
nucancel n (Frac a b) | n <= 0 = (Frac a b)
                     | n > 0 =
    let a' = map (nucancel n) a
        b' = map (nucancel n) b
    in
    fracFromPair (removeMatch (\x y -> (nucancel (n-1)) (umul (Frac [] (delete x a')) (inv x)) == (nucancel (n-1)) (umul (Frac [] (delete y b')) (inv y))) (a',b'))

-- does not obey the cancel condition
ndcancel :: Int -> RFraction -> RFraction
ndcancel n (Frac a b) | n <= 0 = (Frac a b)
                     | n > 0 =
    let a' = map (ndcancel n) a
        b' = map (ndcancel n) b
    in
    fracFromPair (removeMatch (\x y -> (ndcancel (n-1)) (umul (Frac [] a') (inv x)) == (ndcancel (n-1)) (umul (Frac [] b') (inv y))) (a',b'))

cancel :: RFraction -> RFraction
cancel a = ncancel (fractionDepth a) a

--   n = 1
--   r=           [1,2]\2
--   s=           2\[1,2]
--   rs=          [2\[1,2],1,2]\[2\[1,2],1,2]
--   c(r)=        2\
--   c(s)=        2
--   c(r)*c(s)=   2\2
--   c(r*s)=      [2,1,2]\[2,1,2]
--   c(c(r)*c(s))=1
cn2 :: Int -> RFraction -> RFraction
cn2 n r = if n <= 0 then r else 
        let 
        a = map ( cn2 n ) (den r) 
        b = map ( cn2 n ) (nom r) 
        icm (i,j) = cn2 (n-1) ((inv (a !! i)) `hmul` (b !! j)) 
        in
        case find (\(i,j) -> icm (i,j) == cn2 n (Frac (deli i a) (deli j b))) ((indexes a) `x` (indexes b)) of
        Nothing -> Frac a b
        Just p -> icm p
               
-- is not associative
-- Does not obey cancel law with umul, n=1
--   r=           [2,1]\2
--   s=           [2\]\
--   rs=          [[2,1]\[2,1],2,1]\[[2\]\]
--   c(r)=        2\
--   c(s)=        [2\]\
--   c(r)*c(s)=   [2\2,1]\
--   c(r*s)=      [[2,1]\[2,1],2,1]\[[2\]\]
--   c(c(r)*c(s))=[1,1]\
-- Never cancels [2\]\[2\]
cn :: Int -> RFraction -> RFraction
cn n r = if n <= 0 then r else 
        let 
        a = map ( cn n ) (den r) 
        b = map ( cn n ) (nom r) 
        icm (i,j) = cn (n-1) ((inv (a !! i)) `umul` (b !! j)) 
        in
        case find (\(i,j) -> icm (i,j) == cn n (Frac (deli i a) (deli j b))) ((indexes a) `x` (indexes b)) of
        Nothing -> Frac a b
        Just p -> icm p
               
-- c :: RFraction -> RFraction 
-- c r = cn2 (fractionDepth r) r

sle1 :: RSemiFraction -> RSemiFraction -> Bool
sle1 [] b = True
sle1 a [] = a == []
sle1 a b = 
    let (a1:ar) = sort a; (b1:br) = sort b in
        if sle a1 b1 
           then True
           else sle1 ar br

instance Ord RFraction where
    a <= b = sle a b

sle :: RFraction -> RFraction -> Bool
sle (Frac den nom) (Frac den2 nom2) = sle1 (den ++ nom) (den2 ++ nom2)

toTotalDivTree1 :: RSemiFraction -> TotalDivTree.RWord
toTotalDivTree1 [] = [] 
toTotalDivTree1 a = let (a1:ar) = sort a
                        ar' = toTotalDivTree1 ar
                  in
                  (TotalDivTree.L True ((toTotalDivTree a1) ++ (TotalDivTree.invw ar'))) : ar'

toTotalDivTree :: RFraction -> TotalDivTree.RWord
toTotalDivTree (Frac den nom ) =
    let rnom = toTotalDivTree1 nom
        rden = toTotalDivTree1 den
    in  (TotalDivTree.invw rden) ++ rnom

fractionDepth1 :: RSemiFraction -> Int
fractionDepth1 [] = 0
fractionDepth1 a = 1 + (maximum (map (\a' -> fractionDepth a') a))

fractionDepth :: RFraction -> Int
fractionDepth (Frac a b) = fractionDepth1 (a ++ b)

fractionCount1 :: RSemiFraction -> Int
fractionCount1 [] = 0
fractionCount1 a = 1 + sum (map (\a' -> (fractionCount a')) a)

fractionCount :: RFraction -> Int
fractionCount (Frac a b) = fractionCount1 (a ++ b)

leavesCount :: RFraction -> Int
leavesCount (Frac a b) =
    let lc a = if a == [] then 1 else 
               sum (map (\r' -> (leavesCount r')) a) 
    in lc(a) + lc(b)


isStack1 :: RSemiFraction -> Bool
isStack1 a = isStack (Frac [] a)

isStack :: RFraction -> Bool
isStack (Frac [] [] ) = True
isStack (Frac [] (x:[])) = isStack x
isStack (Frac _ _ ) = False

showAsString1 :: RSemiFraction -> String
showAsString1 a =
    if isStack1 a 
       then show (1+(fractionDepth1 a))
       else show a

instance Show RFraction where
    show (Frac [] []) = "1"
    show (Frac [] b) = showAsString1 b
    show (Frac a []) = (showAsString1 a) ++ "\\"
    show (Frac a b)  = (showAsString1 a) ++ "\\" ++ (showAsString1 b)



-- showAsLatex0 :: RSemiFraction -> String
-- showAsLatex0 [] = ""
-- showAsLatex0 [x] = showAsLatex x
-- showAsLatex0 (x:a) = (showAsLatex x) ++ "," ++ (showAsLatex0 a)
-- 
-- showAsLatex1 :: RSemiFraction -> String
-- showAsLatex1 a =
--     if isStack1 a then show (1+(fractionDepth1 a))
--     else "\\phantom{\\lbrack}" ++ (showAsLatex0 a) ++ "\\phantom{\\rbrack}"
-- 
-- -- problems showing [[2\,1]\[[2\]\2,1],1]\[[2,1]\] correctly
showAsLatex :: RFraction -> String
showAsLatex = showAsLatexMatrix "Bmatrix"
-- showAsLatex (Frac [] []) = "1"
-- showAsLatex (Frac [] b) = "\\underline{" ++ (showAsLatex1 b) ++ "}"
-- showAsLatex (Frac a []) = "\\overline{" ++ (showAsLatex1 a) ++ "}"
-- showAsLatex (Frac a b) = "\\cfrac{" ++ (showAsLatex1 b) ++ "}{" ++ (showAsLatex1 a) ++ "}"

showAsLatextf0 :: RSemiFraction -> String
showAsLatextf0 ([]) = ""
showAsLatextf0 ([x]) = showAsLatextf x
showAsLatextf0 (x:a) = (showAsLatextf x) ++ "," ++ (showAsLatextf0 a)

showAsLatextf :: RFraction -> String
showAsLatextf (Frac [] []) = "1"
showAsLatextf (Frac [] b) = 
   if isStack1 b then show (1+(fractionDepth1 b))
   else "\\{" ++ (showAsLatextf0 b) ++ "\\}"
showAsLatextf (Frac a []) = 
   if isStack1 a then  "\\sim" ++ (show (1+(fractionDepth1 a)))
   else "\\sim\\{" ++ (showAsLatextf0 a) ++ "\\}"
showAsLatextf (Frac a b) = 
   "\\tf{" ++ (showAsLatextf0 b) ++ "}{" ++ (showAsLatextf0 a) ++ "}"


showAsLatexMatrix0 :: String -> RSemiFraction -> String
showAsLatexMatrix0 matrixDelim ([]) = ""
showAsLatexMatrix0 matrixDelim ([x]) = showAsLatexMatrix matrixDelim x
showAsLatexMatrix0 matrixDelim (x:a) = (showAsLatexMatrix matrixDelim x) ++ "," ++ (showAsLatexMatrix0 matrixDelim a)

showAsLatexMatrix :: String -> RFraction -> String
showAsLatexMatrix matrixDelim (Frac [] []) = "1"
showAsLatexMatrix matrixDelim (Frac [] b) = 
   if isStack1 b then show (1+(fractionDepth1 b))
   else
  "\\begin{" ++ matrixDelim ++ "}" ++ 
      (showAsLatexMatrix0 matrixDelim b) ++ 
   "\\end{" ++ matrixDelim ++ "}"
showAsLatexMatrix matrixDelim (Frac a []) = 
   if isStack1 a then  "\\sim" ++ (show (1+(fractionDepth1 a)))
   else
   "\\sim\\begin{" ++ matrixDelim ++ "}" ++ 
      (showAsLatexMatrix0 matrixDelim a) ++ 
   "\\end{" ++ matrixDelim ++ "}"
showAsLatexMatrix matrixDelim (Frac a b) = 
   "\\begin{" ++ matrixDelim ++ "}" ++ 
      (showAsLatexMatrix0 matrixDelim b) ++ "\\\\" ++ 
      (showAsLatexMatrix0 matrixDelim a) ++ 
   "\\end{" ++ matrixDelim ++ "}"


showAsSimple0 :: RSemiFraction -> String
showAsSimple0 [] = ""
showAsSimple0 [x] = showAsSimple x
showAsSimple0 (x:a) = (showAsSimple x) ++ "," ++ (showAsSimple0 a)

showAsSimple :: RFraction -> String
showAsSimple (Frac [] []) = "1"
showAsSimple (Frac [] b) =
   if isStack1 b then show (1+(fractionDepth1 b))
   else "[;" ++ (showAsSimple0 b) ++ "]"
showAsSimple (Frac a b) =
   "[" ++ (showAsSimple0 a) ++ ";" ++ (showAsSimple0 b) ++ "]"

rf :: Int -> RFraction
rf 1 = e
rf (n+1) = inc (rf n)

rf1, rf2, rf3, rf4 :: RFraction
rf1 = e
rf2 = inc rf1
rf3 = inc rf2
rf4 = inc rf3
rf5 = inc rf4
rf6 = inc rf5

swap :: (RSemiFraction, RSemiFraction) -> (RSemiFraction,RSemiFraction)
swap (a,b) =
    let a2 = [ ae %* (Frac [] b) | ae <- a ]
        b2 = [ be %* (Frac a2 []) | be <- b ]
    in (b2,a2)

roundswap (a1,b1,c1) =
    let
      ((b2,a2),c2)=(swap(a1,b1),c1)
      (b3,(c3,a3))=(b2,swap(a2,c2))
      ((c4,b4),a4)=(swap(b3,c3),a3)
      (c5,(a5,b5))=(c4,swap(b4,a4))
      ((a6,c6),b6)=(swap(c5,a5),b5)
      (a7,(b7,c7))=(a6,swap(c6,b6))
   in (a7,b7,c7)

    
lastInvElements :: RFraction -> [(Int,RFraction)]
lastInvElements(Frac a []) = 
    [ (ai,(a!!ai) `smul` (Frac (deli ai a) [])) | ai <- indexes a ]

lastInvElements(Frac a b) =
   let qs = bagsCut (map (\b' -> [ x | (_,x) <- lastInvElements b']) b) 
       really q = find (\ai -> (q `smul` (Frac b (deli ai a))) == (a!!ai) ) (indexes a)
   in
   map (\((Just i),q) -> (i,q))
   [ ((really q),q) | q <- qs, case really q of { (Just i) -> True; Nothing -> False } ]
   
    
smul :: RFraction -> RFraction -> RFraction
smul (a@(Frac ab at)) (b@(Frac bb bt)) =
    let (at',bb') = removeMatch (==) (at,bb) 
        (abi,bti) = removeMatch (\(i,x) (j,y) -> x == y) ((lastInvElements a),(lastInvElements (inv b)))
        ab' = [ ab!!i | (i,_) <- abi ]
        bt' = [ bt!!i | (i,_) <- bti ]
    in
    Frac ([ bl `smul` (inv a) | bl <- bb'] ++ ab') 
         ([ au `smul` b       | au <- at'] ++ bt')
