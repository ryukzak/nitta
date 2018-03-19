{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Test.Utils where

import           NITTA.Types
import           NITTA.Utils
import           Test.Tasty.HUnit

values2dumpTests = do
    "0" @=? values2dump [X, X, X, X]
    "0" @=? values2dump [B False, B False, B False, B False]
    "f" @=? values2dump [B True, B True, B True, B True]
    "17" @=? values2dump [B True, B False, B True, B True, B True]
