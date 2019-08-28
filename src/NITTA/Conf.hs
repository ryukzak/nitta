{-# LANGUAGE FlexibleContexts #-}

module NITTA.Conf ( addManual, createMicroarch, endManual ) where

import           Control.Monad.State.Lazy
import           NITTA.Model.Networks.Bus

-- manual add PU
addManual el = get >>= \lst -> put $ lst ++ [el]

-- manual create BusNetwork with width parameter counter
endManual counter ioSync = get >>= (\pu -> return $ busNetwork counter ioSync pu)

-- eval state and create microarch
createMicroarch net = evalState net []


