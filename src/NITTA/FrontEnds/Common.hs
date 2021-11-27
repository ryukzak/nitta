module NITTA.FrontEnds.Common (
    FrontendResult (..),
    TraceVar (..),
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import NITTA.Intermediate.DataFlow

data FrontendResult v x = FrontendResult
    { frDataFlow :: DataFlowGraph v x
    , frTrace :: [TraceVar]
    , frPrettyLog :: [HM.HashMap v x] -> [HM.HashMap String String]
    }

data TraceVar = TraceVar {tvFmt, tvVar :: T.Text}
    deriving (Show)
