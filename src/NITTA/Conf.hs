{-# LANGUAGE FlexibleContexts #-}
module NITTA.Conf ( addManual, createMicroarch ) where
import Control.Monad.State.Lazy

-- manual add PU
addManual el = get >>= \lst -> put $ lst ++ [el]


-- make :: State [String] String
-- make = do
    -- f <- get
    -- put f
    -- add "kek"
    -- add "lol"
    -- add "345"
    -- return (busNetwork)

createMicroarch net = evalState net []
