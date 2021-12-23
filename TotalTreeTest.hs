module TotalTreeTest where
import qualified TotalDivTree
import TotalTree
import QuickCheck

instance Arbitrary TotalTree where
    arbitrary = oneof [ return E, return (LR E E), return (LR E (LR E E)) ]


prop_Translation :: Int -> TotalTree -> TotalTree -> TotalTree -> Property
prop_Translation n a b x = (n >= 1 && n <= 4) ==> hop n (a %* b) x == hop n a (hop n b x)

-- special case of translation n=1
prop_Assoc :: TotalTree -> TotalTree -> TotalTree -> Bool
prop_Assoc x y z = x %* (y %* z) == (x %* y) %* z 

prop_Trans2 a = asTotalDivTree(trans a) == TotalDivTree.trans (asTotalDivTree a)
