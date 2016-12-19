{-# LANGUAGE
StandaloneDeriving, GeneralizedNewtypeDeriving,
ForeignFunctionInterface #-}

module Basic.Doub
  ( Doub (..)
  , (%)
  , module Data.Bits
  )
where

import Data.Bits
import GHC.Read
import System.Random
import Text.Printf  

-- TODO: Need to think about how to handle the fact that 0/0 is -inf in Basic.
--   Could just be a bug that I choose not implement.


-- Interestingly, Chipmunk BASIC seems to implement all numeric types as IEEE
-- Doubles (and makes such a claim in the manual).  For simplicity, this is the
-- only numeric type and is also used for Boolean values.
newtype Doub = D {getDbl :: Double}

-- Let's get all the goodness of Doubles without making an orphan instance for
-- the Bits typeclass
deriving instance Eq         Doub
deriving instance Enum       Doub
deriving instance Floating   Doub
deriving instance Fractional Doub
deriving instance Num        Doub
deriving instance Ord        Doub

deriving instance Real       Doub
deriving instance RealFloat  Doub
deriving instance RealFrac   Doub
deriving instance Random     Doub         

instance Show Doub where
  showsPrec p (D d) =
    showString $ printf "%.4f" d

instance Read Doub where
  readPrec = D <$> readPrec

instance Bits Doub where
  (.&.)        = dblAnd
  (.|.)        = dblOr
  xor          = dblXor
  complement   = dblCmp
  shiftL       = dblShl
  shiftR       = dblShr
  rotateL      = dblRol
  rotateR      = dblRor
  bitSize      = const 64
  bitSizeMaybe = Just . const 64
  isSigned     = const True
  bit          = bitDefault
  testBit      = testBitDefault
  popCount     = popCountDefault

-- Since the Doub type is just a newtype wrapper for Double, it can be
-- marshalled just like Doubles without any extra effort.  This isn't _really_
-- necessary, but it's fun to play around with.
foreign import ccall dblAnd :: Doub -> Doub -> Doub
foreign import ccall dblOr  :: Doub -> Doub -> Doub
foreign import ccall dblXor :: Doub -> Doub -> Doub
foreign import ccall dblCmp :: Doub -> Doub
foreign import ccall dblShl :: Doub -> Int -> Doub
foreign import ccall dblShr :: Doub -> Int -> Doub
foreign import ccall dblRol :: Doub -> Int -> Doub
foreign import ccall dblRor :: Doub -> Int -> Doub
foreign import ccall dblMod :: Doub -> Doub -> Doub


infixl 7 %
(%) :: Doub -> Doub -> Doub      
(%) = dblMod
