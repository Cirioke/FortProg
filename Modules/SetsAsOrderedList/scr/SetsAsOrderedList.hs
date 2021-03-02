-- Credit to Michael Hanus

module SetsAsOrderedList (Set, empty, insert, isElem, union, toList, fromList)
 where

----------------------------------------------------------------------
----------------------- CODE FROM LECTURE-----------------------------
----------------------------------------------------------------------
-- Credit to Michael Hanus
-- Implementing sets as ordered lists
data Set a = Set [a]
  deriving Show

-- Credit to Michael Hanus
empty :: Set a
empty = Set []

-- Credit to Michael Hanus
insert :: Ord a => a -> Set a -> Set a
insert x (Set xs) = Set (oinsert xs)
 where oinsert []     = [x]
       oinsert (y:ys) | x == y    = y : ys
                      | x <  y    = x : y :ys
                      | otherwise = y : oinsert ys
-- > average: consider half of the list elements (linear complexity)

-- Credit to Michael Hanus
isElem :: Ord a => a -> Set a -> Bool
isElem x (Set xs) = oelem xs
 where oelem []     = False
       oelem (y:ys) | x == y    = True
                    | x <  y    = False
                    | otherwise = oelem ys
-- > average: consider half of the list elements (linear complexity)

-- Credit to Michael Hanus
union :: Ord a => Set a -> Set a -> Set a
union (Set xs1) (Set xs2) = Set (ounion xs1 xs2)
 where ounion []     ys     = ys
       ounion (x:xs) []     = x : xs
       ounion (x:xs) (y:ys) | x == y    = x : ounion xs ys
                            | x <  y    = x : ounion xs (y:ys)
                            | otherwise = y : ounion (x:xs) ys
-- > linear complexity w.r.t. the size of both sets!


----------------------------------------------------------------------
----------------------- OWN CODE -------------------------------------
----------------------------------------------------------------------

toList :: Set a -> [a]

fromList :: [a] -> Set a