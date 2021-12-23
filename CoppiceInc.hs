module CoppiceInc  where
class CoppiceInc copse where
    e :: copse
    inc, inv :: copse -> copse
    (%*) :: copse -> copse -> copse
    (%+) :: copse -> copse -> copse
    (%+) = \a b -> (inc (a %* (inv b))) %* b
--     inc = \a -> a %+ e

