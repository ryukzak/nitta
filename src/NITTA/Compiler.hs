{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module NITTA.Compiler where

import           Control.Monad
import           Data.Array              (array)
import           Data.Default
import qualified Data.Graph              as G
import           Data.List               (find, groupBy, intersect, nub,
                                          partition, sortBy, takeWhile)
import           Data.Map                (fromList)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes, fromMaybe, isNothing)
import qualified Data.Text               as T
import           Data.Typeable           (cast, typeOf)
import           Debug.Trace
import           GHC.Generics
import           NITTA.Base
import           NITTA.BusNetwork
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks    as FB
import           NITTA.ProcessUnits
import           NITTA.ProcessUnits.FRAM
import           NITTA.Timeline



naive net = do
  if not $ null $ variants net
    then naiveStep net
    else return $ naiveBind net


naiveStep pu@BusNetwork{..} = do
  let vars = variants pu
  when (length vars == 0) $ error "No variants!"
  -- mapM_ putStrLn $ map ((++) "" . show) vars
  -- putStrLn "-----------------------------"
  let act = v2a $ head vars
  -- putStrLn $ show act
  -- putStrLn "============================="
  return $ step pu act
  where
    -- mostly mad implementation
    v2a NetworkVariant{ vPullAt=TimeConstrain{..}, ..} = NetworkAction
      { aPullFrom=vPullFrom
      , aPullAt=Event tcFrom tcDuration
      , aPush=M.map (fmap $ \(title, TimeConstrain{..}) ->
                        (title, Event pushStartAt tcDuration)
                    ) vPush
      }
      where
        pushStartAt = tcFrom + tcDuration




naiveBind net@BusNetwork{..} =
  let (exclusive, other) = partition ((==) 1 . length . snd) $ bindVariants net
      -- Не будет толком работать в случае, если pull должен мультиплексироваться.
      (restless, other') = partition isRestless other
      (inputs, other'') = partition isInput other'
      (fb, puTitle:_):_ = exclusive ++ restless ++ inputs ++ other''
  in --trace ("bind " ++ show fb ++ " on " ++ puTitle) $
     subBind fb puTitle net
  where
    isInput (fb, (puTitle:_))
      | dependency fb == [] =
          any (\case (PUVar (Pull _) _) -> True
                     otherwise -> False
              ) $ variantsAfterBind (niPus M.! puTitle) fb
      | otherwise = False
    isRestless (fb, (puTitle:_))
      | not $ null (variables fb `intersect` restlessVariables) =
          any (\case (PUVar (Push v) _) | v `elem` restlessVariables -> True
                     otherwise -> False
              ) $ variantsAfterBind (niPus M.! puTitle) fb
      | otherwise = False
    restlessVariables = [ v
      | NetworkVariant{..} <- variants net
      , (v, Nothing) <- M.assocs vPush
      ]

variantsAfterBind pu fb = case bind pu fb of
  Just pu' -> filter (\(PUVar act _) -> act `variantOf` fb)  $ variants pu'
  Nothing  -> []
  where
    act `variantOf` fb = not $ null (variables act `intersect` variables fb)

