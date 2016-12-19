

module Basic.Unparse
  ( module Text.PrettyPrint
  , module Basic.Unparse
  )
  where

import Text.PrettyPrint
import Data.Text (Text, unpack)
import Basic.Doub  

class Unparse a where
  unp :: a -> Doc

pretty :: Unparse a => a -> String             
pretty = render . unp

minus, plus, divide, times, tothe :: Doc         
minus  = char '-'
plus   = char '+'
divide = char '/'
times  = char '*'
tothe  = char '^'

         
instance Unparse Text where
  unp = text . unpack

instance Unparse Int where
  unp = int

instance Unparse Doub where
  unp = text . show

instance Unparse a => Unparse (Maybe a) where
  unp = maybe empty unp
