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

framRegAndOut = unitTestbench "framRegAndOut" framProxy
  (Just def{ cntxVars=M.fromList [("aa", [42]), ("ac", [0x1003])] })
  [ F.reg "aa" ["ab"]
  , F.framOutput 9 "ac"
  ]

framRegAndConstant = unitTestbench "framRegAndConstant" framProxy
  (Just def{ cntxVars=M.fromList [("dzw", [975])] })
  [ F.reg "dzw" ["act","mqt"]
  , F.constant 11 ["ovj"]
  ]



-- Ниже приведённый код раньше использовался для генерации максимально плотной программы для Fram, с
-- учётом особенностей внутренней диспетчеризации. Остальная часть алгоритма - в истории.
--
-- TODO: Необходимо реимплементировать для новых условий в виде через создание контролируемого
-- processGen и сортировки / фильтрации алгоритма.
--
-- data ST = ST { acc           :: [F Parcel String]
--              , forInput      :: [Int]
--              , forOutput     :: [Int]
--              , numberOfLoops :: Int
--              , usedVariables :: [String]
--              , values        :: [(String, Int)]
--              } deriving (Show)
--
-- framDataFlowGen checkCellUsage generalPred =
--   suchThat (do
--                size <- sized pure -- getSize
--                n <- choose (0, size)
--                foldM maker (ST [] [] [] 0 [] []) [ 0..n ]
--            ) generalPred
--   where
--     maker st0@ST{..} _ = nextState st0 <$> do
--       fb <- suchThat (oneof [ F <$> (arbitrary :: Gen (FramInput Parcel String))
--                             , F <$> (arbitrary :: Gen (FramOutput Parcel String))
--                             , F <$> (arbitrary :: Gen (Loop Parcel String))
--                             , F <$> (arbitrary :: Gen (Reg Parcel String))
--                             ]
--                      ) check
--       v <- choose (0 :: Int, 0xFF)
--       return (fb, v)
--         where
--           nextState st (fb, v) = specificUpdate fb v st
--             { acc=fb : acc
--             , usedVariables=variables fb ++ usedVariables
--             }
--           specificUpdate fb value st
--             | Just (FramInput addr _vs) <- castF fb = st{ forInput=addr : forInput }
--             | Just (FramOutput addr (I v)) <- castF fb = st{ forOutput=addr : forOutput
--                                                             , values=(v, value) : values
--                                                             }
--             | Just (Loop _bs (I a)) <- castF fb = st{ numberOfLoops=numberOfLoops + 1
--                                                      , values=(a, value) : values
--                                                      }
--             | Just (Reg (I a) _bs) <- castF fb = st{ values=(a, value) : values }
--             | otherwise = error $ "Bad F: " ++ show fb
--           check fb
--             | not $ null (variables fb `intersect` usedVariables) = False
--             | Just (Reg _ _ :: Reg Parcel String) <- castF fb = True
--             | not checkCellUsage = True
--             | not (dfIoUses < framDefSize) = False
--             | Just (FramInput addr _) <- castF fb = addr `notElem` forInput
--             | Just (FramOutput addr _) <- castF fb = addr `notElem` forOutput
--             | otherwise = True -- for Loop
--           dfIoUses = length (nub $ forInput `union` forOutput) + numberOfLoops
