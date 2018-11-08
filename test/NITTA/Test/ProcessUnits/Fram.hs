{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Test.ProcessUnits.Fram where

import           Data.Default
import qualified Data.Map                as M
import           Data.Proxy
import qualified NITTA.Functions         as F
import           NITTA.ProcessUnits.Fram
import           NITTA.Test.Functions    ()
import           NITTA.Test.ProcessUnits
import           NITTA.Types


framProxy = Proxy :: Proxy (Fram String Int Int)

-----------------------------------------------------------

framRegAndOut = unitTestBench "framRegAndOut" framProxy
    (Just def{ cntxVars=M.fromList [("aa", [42]), ("ac", [0x1003])] })
    [ F.reg "aa" ["ab"]
    , F.framOutput 9 "ac"
    ]

framRegAndConstant = unitTestBench "framRegAndConstant" framProxy
    (Just def{ cntxVars=M.fromList [("dzw", [975])] })
    [ F.reg "dzw" ["act","mqt"]
    , F.constant 11 ["ovj"]
    ]
