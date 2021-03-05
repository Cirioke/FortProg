{-# LANGUAGE TemplateHaskell #-}



module TestSets where

import Test.QuickCheck
import SetsAsOrderedList

----------------------------------
-- Copy Paste from Lecture
----------------------------------

-- The type of elements for testing:
type Elem = Int

-- ADT equations:

prop_empty_elem :: Elem -> Bool
prop_empty_elem x = not (isElem x empty)

prop_insert_elem :: Elem -> Set Elem -> Bool
prop_insert_elem x s = isElem x (insert x s)

prop_insert2_elem :: Elem -> Elem -> Set Elem -> Bool
prop_insert2_elem x y s = isElem x (insert y (insert x s))

prop_union :: Elem -> Set Elem -> Set Elem -> Bool
prop_union x s1 s2 =
  isElem x (union s1 s2) == (isElem x s1 || isElem x s2)

----------------------------------
-- Own Code
----------------------------------

-- \ Some helper function to treat lists like sets,
-- is_subs determines wether all elements of one list are in anotherone.
is_subs :: Eq a => [a] -> [a] -> Bool
is_subs []     _   = True
is_subs (x:xs) lst = (x `elem` lst) &&  (xs `is_subs` lst)

-- \ Some helper function to treat lists like sets,
-- is_set_eq determines wether all elements of one list are in anotherone
-- and the other way around.
is_set_eq :: Eq a => [a] -> [a] -> Bool
is_set_eq l0 l1 = (l0 `is_subs` l1) && (l1 `is_subs` l0)



prop_unionMap ::[Set Elem] -> Bool
prop_unionMap sets =
  (length.toList) (unionAll sets) <= sum (map (length.toList) sets)


-- prop_fromList0 :: Bool
-- prop_fromList0 = (fromList []) == empty

prop_fromList1 :: [Elem] -> Bool
prop_fromList1 lst = allIn lst
  where set = fromList lst
        allIn [] = True
        allIn (e:es) = (e `isElem` set) && (allIn es)
        

-- prop_toList0 :: Bool
-- prop_toList0 = (toList empty) == []

prop_fromToList1 :: Set Elem -> Bool
prop_fromToList1 set = (fromList.toList) set == set


-- Executing all tests:
return []
testAll:: IO Bool
testAll = $quickCheckAll