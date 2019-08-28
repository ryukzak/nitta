{-# LANGUAGE FlexibleContexts #-}
module NITTA.Conf ( addManual, createMicroarch, endManual ) where
import Control.Monad.State.Lazy
import           NITTA.Model.Networks.Bus
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Project                    (TargetSynthesis (..),
                                                   mkModelWithOneNetwork,
                                                   runTargetSynthesis)
import           NITTA.UIBackend

-- manual add PU
addManual el = get >>= \lst -> put $ lst ++ [el]

-- manual create BusNetwork with width parameter counter
endManual counter ioSync = get >>= (\pu -> return $ busNetwork counter ioSync pu)

-- eval state and create microarch
createMicroarch net = evalState net []

-- TODO: do one function create microarch 

