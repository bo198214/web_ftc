-- Black box tests on the interface of TotalDivTree
module TotalDivTreeTest where
import QuickCheck
import TotalDivTree

prop_Inv, prop_TransTrans, prop_InvInv, prop_E :: TotalDivTree -> Bool
prop_Assoc :: TotalDivTree -> TotalDivTree -> TotalDivTree -> Bool

prop_E a = e %* a == a
prop_Inv a = (inv a) %* a == e
prop_Assoc a b c = ( a %* b ) %* c == a %* ( b %* c )

-- derived properties

prop_TransTrans a = trans (trans a) == a
prop_InvInv a = inv (inv a) == a 

-- left-commutativity
prop_lco :: RWord -> RWord -> Bool
prop_lco u v = isLCO [(L True (u++[L False v])),(L True v),(L False u),(L False (v++[L False u]))]

instance Arbitrary Letter where
    arbitrary = oneof [ (return (head w2)), (return (head w3)), (return (head w4)), (return (head w5)) ]

smallwords :: Gen RWord
smallwords = vector 3
-- oneof [ (return w1), (return w2), (return w3)]
prop_lco2 = forAll smallwords $ (\u -> forAll smallwords $ (\v -> (prop_lco u v)))

prop_circTrans :: RWord -> RWord -> RWord -> Property
prop_circTrans u v w = ((isLCO (u ++ v)) && (isLCO ((invw v) ++ w))) ==> (isLCO (u ++ w))

prop_circTrans2 = forAll (three smallwords) $ (\(u,v,w) -> prop_circTrans u v w)
