{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Variables
import Type


-- Check all properties in this module:
return []
testAll = $quickCheckAll