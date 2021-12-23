module Test (test) where
import List

test :: Show a => (a -> Bool) -> [a] -> String
test p args =
    case find (not . p) args of
    Nothing -> show True
    Just t -> "Failed with: " ++ show t
