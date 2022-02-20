
module Halogen.Util where

import Halogen.HTML.Properties as HP
import Halogen.HTML as HH

classProp :: forall r a. String -> HP.IProp (class :: String | r) a
classProp cl = HP.class_ (HH.ClassName cl)

