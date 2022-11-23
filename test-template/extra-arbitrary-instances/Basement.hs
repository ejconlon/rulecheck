module Rulecheck.Generated.ArbitraryInstances where

import Basement.UArray
import Basement.Block
import Basement.Types.OffsetSize
import qualified Basement.From
import qualified Basement.Imports
import qualified Basement.UTF8.Base
import Control.DeepSeq
import Control.Exception
import Data.Proxy
import Foreign.Ptr
import GHC.Word
import Test.QuickCheck (Arbitrary (..), Testable (..), (===))

instance Arbitrary Basement.Imports.String where
  arbitrary = fmap Basement.Imports.fromString arbitrary

instance NFData Basement.Imports.String where
  rnf (Basement.UTF8.Base.String a) = seq a ()

instance Arbitrary (Ptr ()) where
  arbitrary = return nullPtr

instance (Arbitrary a, PrimType a) => Arbitrary (UArray a) where
  arbitrary = fmap (Basement.From.from :: Block a -> UArray a) arbitrary

instance (Arbitrary a, PrimType a) => Arbitrary (Block a) where
  arbitrary = fmap Basement.Imports.fromList arbitrary


instance NFData (UArray a) where
  rnf (UArray a b _) = seq (a, b) ()

instance NFData (CountOf a) where
  rnf (CountOf x) = seq x ()

instance NFData a => NFData (Offset a) where
  rnf (Offset x) = seq x ()
