module TotalDivTree 
    (
     w1, w2, w3, w4, w5, w6, w7, w8,
     t1, t2, t3, t4, t5, t6, t7, t8,
     e, inc, inv, (%*),
     tWordList, cWordList, bwWordList, numwordlist, 
     wordDepth, invw, trans, asRWord, cancel, RWord, Letter(L), TotalDivTree(C), CLetter(E), isTO, isLCO
    ) where
-- import Test.QuickCheck
import General
import List
import CoppiceInc
-- instance CoppiceInc Tc where
-- why must not Tc be a type synonym?

data Letter = L Bool RWord deriving Eq
type RWord = [ Letter ]

-- reduced or canceled RWord
data CLetter = E Bool TotalDivTree deriving Eq
newtype TotalDivTree = C [ CLetter ] deriving Eq

instance CoppiceInc TotalDivTree where
--     e :: TotalDivTree
--     inv, inc :: TotalDivTree -> TotalDivTree
--     (%*), (-*) :: TotalDivTree -> TotalDivTree -> TotalDivTree
    e = C [] 
    inv (C []) = C []
    inv (C ((E ap a1):ar)) = inv(C ar) %++ (C [E (not ap) a1])
    inc a  = C [E True a] 
    a %* b = (inv a) -* b

isCanceled :: RWord -> Bool
isCanceled [] = True
isCanceled [l] = True
isCanceled w = not ( any (\i -> 
                         let 
                         (L ap aw) = (w!!(i-1))
                         (L bp bw) = (w!!i)
                         in
                         (ap == not bp) && (aw == bw ) 
                    ) (drop 1 (indexes w)))

cancel :: RWord -> TotalDivTree
cancel [] = C []
cancel [ L ap a ] = C [ E ap (cancel a) ]
cancel a = let k = div (length a) 2 in (cancel (take k a)) %* (cancel (drop k a))

asRWord :: TotalDivTree -> RWord
asRWord (C []) = []
asRWord (C ((E ap a1):ar)) = (L ap (asRWord a1)):asRWord(C ar)

(%++) :: TotalDivTree -> TotalDivTree -> TotalDivTree
(C a) %++ (C b) = C ( a ++ b )

(C []) -* b = b
a -* (C []) = inv a
(C((E s a1):ar)) -* (C((E t b1):br)) = 
    if (s == t) && a1 == b1 then (C ar) -* (C br)
    else  inv(C ar) %++ (C ([E (not s) a1] ++ [E t b1] ++ br))


-- instance Eq L where
--     (E ap [] ) == (E bp b) = ( ap == bp ) && [] == b
--     (E ap a ) == (E bp []) = ( ap == bp ) && a == []
--     (E ap (a1:ar)) == (E bp (b1:br)) = ( ap == bp ) && (a1 == b1) && (ar == br)

-- Some Constants

t1 = C []
t2 = C [E True t1]
t3 = C [E True t2]
t4 = C [E True t3]
t5 = C [E True t4]
t6 = C [E True t5]
t7 = C [E True t6]
t8 = C [E True t7]

w1 = []
w2 = [L True w1]
w3 = [L True w2]
w4 = [L True w3]
w5 = [L True w4]
w6 = [L True w5]
w7 = [L True w6]
w8 = [L True w7]

-- equivalence relations on RWords

oarc :: RWord -> Int -> Int -> RWord
oarc w k m =
    if k < m then take ((m-k)-1) (drop (k+1) w)
    else (drop (k+1) w) ++ (take m w)

isPos :: Letter -> Bool
isPos (L ap a) = ap
word (L ap a) = a

isTO :: RWord -> Bool
isTO [] = True
isTO w = 
    let
    posIndex = findIndices isPos w
    negIndex = findIndices (not . isPos) w
    in
    existBij (\i j -> 
              isTO (oarc w i j)
              && isTO (invw (word (w!!i)) ++ (word (w!!j)))
             ) posIndex negIndex 

isLCO w =
    let
    posIndex = findIndices isPos w
    negIndex = findIndices (not . isPos) w
    in
    existBij (\i j -> 
              isLCO ((word (w!!i)) ++ (oarc w i j) ++ (invw(word (w!!j))))
             ) posIndex negIndex

    
    
invw :: RWord -> RWord
invw w = map (\(L p w') -> (L (not p) w')) (reverse w)

-- Output 
isStack :: RWord -> Bool
isStack [] = True
isStack ([(L s a1)]) = if s == True then isStack(a1) else False 
isStack ( _ : _ ) = False

wordDepth :: RWord -> Int
wordDepth [] = 0
wordDepth ((L ap a1):ar) = max (1 + wordDepth a1) (wordDepth ar)

numwordlist :: Int -> Int -> Integer
numwordlist 0 _ = 1
numwordlist d b | d > 0 = 
  let 
  deptharrays = concat (map (\b' -> prod (replicate b' [0..(d-1)])) [1..b])
  in
  sum (map (\da -> product (map (\d' -> 2*(numwordlist d' b)) da)) deptharrays)

-- all words with at most depth d and at most breadth b
tWordList ::  Int -> Int -> [ RWord ]
tWordList 0 _ = [[]]
tWordList d b | d > 0 = 
  let 
  deptharrays = concat (map (\b' -> prod (replicate b' [0..(d-1)])) [1..b])
  letterList = (\d' -> 
                 (map (L True)  (tWordList (d') b) ++ 
                  (map (L False) (tWordList (d') b))) 
            )
  in
  concat (map (\da -> prod (map letterList da) ) deptharrays)

cWordList :: Int -> Int -> [ RWord ]
cWordList 0 _ = [[]]
cWordList d b | d > 0 = 
  let 
  deptharrays = concat (map (\b' -> prod (replicate b' [0..(d-1)])) [1..b])
  letterList = (\d' -> 
                 (map (L True)  (cWordList (d') b) ++ 
                  (map (L False) (cWordList (d') b))) 
            )
  in
  filter (\w -> isCanceled w) (concat (map (\da -> prod (map letterList da) ) deptharrays))

bwWordList :: Int -> Int -> [ RWord ]
bwWordList 0 _ = [[]]
bwWordList d b | d > 0 =
  let 
  deptharrays = concat (map (\b' -> prod (replicate b' [0..(d-1)])) [1..b])
  negLetterList = (\d' -> (map (L False) (bwWordList (d') b))) 
  posLetterList = (\d' -> (map (L True)  (bwWordList (d') b))) 
  in
  concat (map (\da -> concat( map (\m -> prod (
    (map negLetterList (take m da)) ++
    (map posLetterList (drop m da))
                                              )) [0..(length da)])) deptharrays)
cbwWordList :: Int -> Int -> [ RWord ]
cbwWordList 0 _ = [[]]
cbwWordList d b | d > 0 =
  let 
  deptharrays = concat (map (\b' -> prod (replicate b' [0..(d-1)])) [1..b])
  negLetterList = (\d' -> (map (L False) (cbwWordList (d') b))) 
  posLetterList = (\d' -> (map (L True)  (cbwWordList (d') b))) 
  in
  filter (\w -> isCanceled w) 
             (concat (map (\da -> concat( map (\m -> prod (
               (map negLetterList (take m da)) ++
               (map posLetterList (drop m da))
                                                          )
                                              ) [0..(length da)])
                          ) deptharrays))

showAsHaskell1 :: Letter -> String
showAsHaskell1 (L ap a1) = "(L " ++ (show ap) ++ " " ++ (showAsHaskell a1) ++ ")"
showAsHaskell :: RWord -> String
showAsHaskell [] = "[]"
showAsHaskell [a1] = "[" ++ (showAsHaskell1 a1) ++ "]"
showAsHaskell (a1:ar) =  "[" ++ (showAsHaskell1 a1) ++ "," ++ (showAsHaskell ar) ++ "]"
-- showAsLatex :: RWord -> String

trans1 :: CLetter -> TotalDivTree
trans1 (E True a) = (C [ E True (inv (trans a)) ]) %* (trans a)
trans1 (E False a) = inv (trans1 (E True a))

trans :: TotalDivTree -> TotalDivTree
trans (C []) = (C [])
trans (C (a1:ar)) = (trans1 a1) %* (trans (C ar))

instance Show Letter where
    show (L s a) = 
        let 
        sshow = if isStack a then show (1 + (wordDepth a)) else show a
        in
        if s == True 
        then "+" ++ sshow
        else "-" ++ sshow
    
instance Show CLetter where
    show (E ap a1) = show(L ap (asRWord a1))

instance Show TotalDivTree where
    show (C a) = show a

