
import Test.QuickCheck

import Variables
import SLDTree


freshCombs = [Comb cName lst | cName <-(map ('f':) freshVars)]

prop_0 :: Rule -> Bool
prop_0 r@(Rule c as) = lenght childs
  where g = Goal c
        p = Prog (c:as)
        SLDTree _ childs = sld p g

Regel 1:    append([]   ,L,L     ) .
Regel 2:    append([E|R],L,[E|RL]) :- append(R,L,RL).