module TotalTree ( trans, (%+), (%*), asTotalDivTree, hop, TotalTree(E,LR) ) where
import qualified TotalDivTree

data TotalTree = E | LR TotalTree TotalTree  deriving (Eq,Show)

hop :: Int -> TotalTree -> TotalTree -> TotalTree
hop 0 a b = LR a b
hop n E x | n>0 = x
hop n (LR a b) x | n>0 = hop (n-1) (hop n a x) (hop n b x)

a %+ b = hop 0 a b
a %* b = hop 1 a b
a %^ b = hop 2 a b

toNat :: TotalTree -> Int
toNat E = 1
toNat (LR a b) = (toNat a) + (toNat b)

trans E = E
trans (LR a b) = LR (trans b) (trans a)

-- embedding to TotalDivTree
asTotalDivTree :: TotalTree -> TotalDivTree.TotalDivTree
asTotalDivTree E = TotalDivTree.e
asTotalDivTree (LR a b) = 
    let 
    b' = asTotalDivTree b 
    a' = asTotalDivTree a
    in
     (a' TotalDivTree.%* (TotalDivTree.inv b')) TotalDivTree.%* b'

