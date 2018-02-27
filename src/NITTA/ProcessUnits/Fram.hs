{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-|
Вычислительный блок fram является одним из наиболее простых блоков с точки хрения аппаратной
реализации. Его внутренее устройство представляет из себя:

- набор входных регистров для защёлкивания входных сначений (как сигналов, так и данных);
- массив регистров, в который могут быть сохранены входные данные.

Но не смотря простоту с аппаратной точки зрения, он имеет весьма высокую сложность с точки зрения
использования прикладного алгоритма. Эта сложность складывается из:

- многофункциональности (fram может в момент написания этого текста выполняет следующие функции:
  FramInput, FramOutput, Reg, Loop, Constant);
- параллелизма (fram может в один момент времени выполнять множество различных функций);
- историчности:

    - fram является statefull вычислительным блоком относительно выполняемых функций, как следствие,
      требует инициализации и работы с состоянием при моделировании / тестировании;
    - fram имеет внутрении ресурсы, ячейки памяти, занятие которых накладывает ограничения на
      функциональные возможности вычислительного блока. Каждая ячейка памяти определяет следующие
      ресурсы:

        - передача данных с предыдущега вычислительного цикла;
        - текущее хранимое значение в рамках вычислительного цикла;
        - передача данных на следующий вычислительный цикл.

Именно по этому реализация модели вычислительного блока настолько велика и сложна. В её рамках были
установлены следующие инварианты, необходимые для корректной работы (по видимому, они должны быть
распространены на все вычислительные блоки):

- Функция bind работает безопастно относительно функций options и decision. Другими словами, если
  функции привязаны к вычислительному блоку, то они могут быть вычислены.
- Функция bind работает небезопастно относительно самой себя. Другими словами, привязка одной
  функции может заблокировать привязку другой функции.
- Функция options работает безопастно. Другими словами, она предоставляет только такие варианты
  решений, при которых гарантируется, что все загруженные функции могут быть выполнены. И это
  гарантируется вне зависимости от очерёдности принятия решений (само собой вопрос эффективности
  умалчивается).
- Функция decision работает небезопастно по отношению к функции bind. Другими словами, принятые
  решения может ограничить функциональность вычислительного блока и запретить привязку функций.

TODO: Каким образом необходимо работать со внутренними ресурсами в условиях разветвлённого времени?
Не получится ли так, что один ресурс будет задействован дважды в разных временных линиях?
-}
module NITTA.ProcessUnits.Fram
  ( Fram(..)
  , FSet(..)
  , Link(..)
  ) where

import           Control.Monad         ((>=>))
import           Data.Array
import           Data.Bits
import           Data.Default
import           Data.Either
import           Data.Foldable
import           Data.Generics.Aliases (orElse)
import           Data.List             (find)
import qualified Data.Map              as M
import           Data.Maybe
import qualified Data.String.Utils     as S
import           Data.Typeable         (cast)
import           NITTA.Compiler
import           NITTA.FunctionBlocks
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval      ((...))



data Fram v t = Fram
  { frMemory   :: Array Int (Cell v t)
  -- | Информация о функциональных блоках, которые необходимо обработать fram-у. Требуют хранения
  -- дополнительной информации, такой как время привязки функционального блока. Нельзя сразу делать
  -- привязку к ячейке памяти, так как это будет неэффективно.
  , frRemains  :: [ (FSet (Fram v t), ProcessUid) ]
  , frBindedFB :: [ FB (Parcel v) v ]
  , frProcess  :: Process v t
  , frSize     :: Int
  } deriving ( Show )

instance ( Default t ) => Default (Fram v t) where
  def = Fram { frMemory=listArray (0, defaultSize - 1) cells
             , frBindedFB=[]
             , frRemains=[]
             , frProcess=def
             , frSize=defaultSize
             }
    where
      defaultSize = 16
      cells = map (\(i, c) -> c{ initialValue=0x1000 + i }) $ zip [0..] $ repeat def

instance WithFunctionalBlocks (Fram v t) (FB (Parcel v) v) where
  functionalBlocks Fram{..} = frBindedFB



instance FunctionalSet (Fram v t) where
  data FSet (Fram v t)
    = FramInput' (FramInput (Parcel v))
    | FramOutput' (FramOutput (Parcel v))
    | Loop' (Loop (Parcel v))
    | Reg' (Reg (Parcel v))
    | Constant' (Constant (Parcel v))
    deriving ( Show, Eq )

instance ( Var v ) => WithFunctionalBlocks (FSet (Fram v t)) (FB (Parcel v) v) where
  -- TODO: Сделать данную операцию через Generics.
  functionalBlocks (FramInput' fb)  = [ FB fb ]
  functionalBlocks (FramOutput' fb) = [ FB fb ]
  functionalBlocks (Loop' fb)       = [ FB fb ]
  functionalBlocks (Reg' fb)        = [ FB fb ]
  functionalBlocks (Constant' fb)   = [ FB fb ]

instance ( Var v ) => ToFSet (Fram v t) v where
  toFSet (FB fb0)
    | Just fb@(Constant _ _) <- cast fb0 = Right $ Constant' fb
    | Just fb@(Reg _ _) <- cast fb0 = Right $ Reg' fb
    | Just fb@(Loop _ _) <- cast fb0 = Right $ Loop' fb
    | Just fb@(FramInput _ _) <- cast fb0 = Right $ FramInput' fb
    | Just fb@(FramOutput _ _) <- cast fb0 = Right $ FramOutput' fb
    | otherwise = Left $ "Fram don't support " ++ show fb0

isReg (Reg' _) = True
isReg _        = False

isConstOrLoop (Constant' _) = True
isConstOrLoop (Loop' _)     = True
isConstOrLoop _             = False


---------------------------------------------------------------------


-- | Описание отдельной ячейки памяти.
data Cell v t = Cell
  { input        :: IOState v t -- ^ Ячейка позволяет получить значения с предыдущего вычислительного цикла.
  , current      :: Maybe (Job v t) -- ^ Ячейка в настоящий момент времени используется для работы.
  , output       :: IOState v t -- ^ Ячейка позволяет передать значение на следующий вычислительный цикл.
  , lastWrite    :: Maybe t -- ^ Момент последней записи в ячейку (необходим для корректной работы с задержками).
  , initialValue :: Int -- ^ Значение ячейки после запуска системы (initial секции).
  } deriving ( Show )

instance Default (Cell v t) where
  def = Cell Undef Nothing Undef Nothing 0



-- | Описание состояния ячейки относительно начала (Input) и конца (Output) вычислительного цикла.
data IOState v t
  = Undef -- ^ Ячейка никак не задействована.
  | Def (Job v t) -- ^ Ячейка будет использоваться для взаимодействия на границе вычислительного цикла.
  | UsedOrBlocked -- ^ Ячейка либо зарезервирована для использования, либо не может быть использована.
  deriving ( Show, Eq )



-- | Данные, необходимые для описания работы вычислительного блока.
data Job v t
  = Job { -- | Хранение информации для последующего фиксирования межуровневых взаимосвязей между
          -- шанами вычислительного процесса.
          cads, endpoints, instructions :: [ ProcessUid ]
          -- | Время начала выполнения работы.
        , startAt                       :: Maybe t
          -- | Функция, выполняемая в рамках описываемой работы.
        , functionalBlock               :: FSet (Fram v t)
          -- | Список действие, которые необходимо выполнить для завершения работы.
        , actions                       :: [ EndpointType v ]
        }
  deriving ( Show, Eq )

instance Default (Job v t) where
  def = Job def def def def undefined def




-- | Предикат, определяющий время привязки функции к вычислительному блоку. Если возвращается
-- Nothing - то привязка выполняеся в ленивом режиме, если возвращается Just адрес - то привязка
-- должна быть выполнена немедленно к указанной ячейки.
immidiateBindTo (FramInput' (FramInput addr _))   = Just addr
immidiateBindTo (FramOutput' (FramOutput addr _)) = Just addr
immidiateBindTo _                                 = Nothing


-- | Привязать функцию к указанной ячейке памяти, сформировав описание работы для её выполнения.
bindToCell cs fb@(FramInput' (FramInput _ (O a))) c@Cell{ input=Undef }
  = Right c{ input=Def def{ functionalBlock=fb
                          , cads=cs
                          , actions=[ Source a ]
                          }
           }
bindToCell cs fb@(FramOutput' (FramOutput _ (I b))) c@Cell{ output=Undef }
  = Right c{ output=Def def{ functionalBlock=fb
                           , cads=cs
                           , actions=[ Target b ]
                           }
           }
bindToCell cs fb@(Reg' (Reg (I a) (O b))) c@Cell{ current=Nothing, .. }
  | output /= UsedOrBlocked
  = Right c{ current=Just $ def{ functionalBlock=fb
                               , cads=cs
                               , actions=[ Target a, Source b ]
                               }
           }
bindToCell cs fb@(Loop' (Loop (O b) (I a))) c@Cell{ input=Undef, output=Undef }
  = Right c{ input=Def def{ functionalBlock=fb
                          , cads=cs
                          , actions=[ Source b, Target a ]
                          }
           }
-- Всё должно быть хорошо, так как если ячейка ранее использовалась, то input будет заблокирован.
bindToCell cs fb@(Constant' (Constant x (O b))) c@Cell{ input=Undef, current=Nothing, output=Undef }
  = Right c{ current=Just $ def{ functionalBlock=fb
                               , cads=cs
                               , actions=[ Source b ]
                               }
           , input=UsedOrBlocked
           , output=UsedOrBlocked
           , initialValue=x
           }
bindToCell _ fb cell = Left $ "Can't bind " ++ show fb ++ " to " ++ show cell



instance ( IOType (Parcel v) v
         , Var v
         , Time t
         , WithFunctionalBlocks (Fram v t) (FB (Parcel v) v)
         ) => ProcessUnit (Fram v t) v t where
  bind fb0 pu@Fram{..} = do fb' <- toFSet fb0
                            pu' <- bind' fb'
                            if isSchedulingComplete pu'
                              then Right pu'
                              else Left "Schedule can't complete stop."
    where
      bind' fb | Just addr <- immidiateBindTo fb
               , let cell = frMemory ! addr
               , let (cad, frProcess') = modifyProcess frProcess $ bindFB fb0 $ nextTick frProcess
               , Right cell' <- bindToCell [cad] fb cell
               = Right pu{ frProcess=frProcess'
                         , frMemory=frMemory // [(addr, cell')]
                         , frBindedFB=fb0 : frBindedFB
                         }

               | Right _ <- bindToCell def fb def
               , let (cad, frProcess') = modifyProcess frProcess $ bindFB fb0 $ nextTick frProcess
               = Right pu{ frProcess=frProcess'
                         , frRemains=(fb, cad) : frRemains
                         , frBindedFB=fb0 : frBindedFB
                         }

               | otherwise = Left ""

  process = frProcess
  setTime t fr@Fram{..} = fr{ frProcess=frProcess{ nextTick=t } }



instance ( Var v, Time t
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Fram v t)
         where

  options _proxy pu@Fram{ frProcess=Process{..}, ..} = fromCells ++ fromRemain
    where
      fromRemain = [ EndpointO ep $ constrain c ep
                   | (fb, cad) <- frRemains
                   , not (isReg fb) || isSourceBlockAllow
                   , (c, ep) <- toList $ do
                       (_addr, cell) <- findCell pu fb
                       cell' <- bindToCell [cad] fb cell
                       ep <- cellEndpoints False cell'
                       return (cell', ep)
                   ]

      fromCells = [ EndpointO ep $ constrain cell ep
                  | (_addr, cell@Cell{..}) <- assocs frMemory
                  , ep <- toList $ cellEndpoints isTargetBlockAllow cell
                  ]

      -- | Загрузка в память значения на следующий вычислительный цикл не позволяет использовать её
      -- в качестве регистра на текущем цикле.
      isTargetBlockAllow = let need = length $ filter (isReg . fst) frRemains
                               allow = length $ filter (\Cell{..} -> output /= UsedOrBlocked) $ elems frMemory
                               reserved = length $ filter (isConstOrLoop . fst) frRemains
                               in need == 0 || allow - reserved > 1
      isSourceBlockAllow = let reserved = length (filter (isConstOrLoop . fst) frRemains)
                               allow = length $ filter (\Cell{..} -> input == Undef && output == Undef) $ elems frMemory
                               in reserved == 0 || reserved < allow

      constrain Cell{..} (Source _)
        | lastWrite == Just nextTick = TimeConstrain (nextTick + 1 ... maxBound) (1 ... maxBound)
        | otherwise              = TimeConstrain (nextTick ... maxBound) (1 ... maxBound)
      constrain _cell (Target _) = TimeConstrain (nextTick ... maxBound) (1 ... maxBound)


  decision proxy pu@Fram{ frProcess=p@Process{ nextTick=tick0 }, .. } act@EndpointD{..}
    | isTimeWrap p act = timeWrapError p act

    | Just (fb, cad1) <- find ( anyInAction . variables . fst ) frRemains
    = either error id $ do
        (addr, cell) <- findCell pu fb

        let (cad2, p') = modifyProcess p $ bind2CellStep addr fb tick0
        cell' <- bindToCell [cad1, cad2] fb cell
        let pu' = pu{ frRemains=filter ((/= fb) . fst) frRemains
                    , frMemory=frMemory // [(addr, cell')]
                    , frProcess=p'
                    }
        return $ decision proxy pu' act

    | Just (addr, cell) <- find ( any (<< epdType) . cellEndpoints True . snd ) $ assocs frMemory
    = case cell of
        Cell{ input=Def job@Job{ actions=a : _ } } | a << epdType
          ->  let (p', job') = schedule addr job
                  cell' = updateLastWrite (nextTick p') cell
                  cell'' = case job' of
                    Just job''@Job{ actions=Target _ : _, functionalBlock=Loop' _ }
                      -- Данная ветка работает в случае Loop. "Ручной" перенос работы необходим для
                      -- сохранения целостности описания вычислительного процесса.
                      -> cell'{ input=UsedOrBlocked, output=Def job'' }
                    Just job''@Job{ actions=Source _ : _ } -> cell{ input=Def job'' }
                    Just _ -> error "Fram internal error after input process."
                    Nothing -> cell'{ input=UsedOrBlocked }
              in pu{ frMemory=frMemory // [(addr, cell'')]
                   , frProcess=p'
                   }
        Cell{ current=Just job@Job{ actions=a : _ } } | a << epdType
          ->  let (p', job') = schedule addr job
                  cell' = updateLastWrite (nextTick p') cell
                  cell'' = cell'{ input=UsedOrBlocked
                                , current=job'
                                }
              in pu{ frMemory=frMemory // [(addr, cell'')]
                   , frProcess=p'
                   }
        Cell{ output=Def job@Job{ actions=act1 : _ } } | act1 << epdType
          ->  let (p', Nothing) = schedule addr job
                  -- FIXME: Eсть потенциальная проблема, которая может встречаться и в других
                  -- вычислительных блоках. Если вычислительный блок загружает данные в последний
                  -- такт вычислительного цикла, а выгружает их в первый так, то возможно ситуация,
                  -- когда внутрение процессы не успели завершиться. Решение этой проблемы должно
                  -- лежать в плоскости метода process, в рамках которого должен производиться
                  -- анализ уже построенного вычислительного процесса и в случае необходимости,
                  -- добавляться лишний так простоя.
                  cell' = cell{ input=UsedOrBlocked
                              , output=UsedOrBlocked
                              }
              in pu{ frMemory=frMemory // [(addr, cell')]
                   , frProcess=p'
                   }
        _ -> error "Fram internal decision error."

    | otherwise = error $ "Can't found selected action: " ++ show act
                  ++ " tick: " ++ show (nextTick p) ++ "\n"
                  ++ "available options: \n" ++ concatMap ((++ "\n") . show) (options endpointDT pu)
                  ++ "cells:\n" ++ concatMap ((++ "\n") . show) (assocs frMemory)
                  ++ "remains:\n" ++ concatMap ((++ "\n") . show) frRemains
    where
      anyInAction = any (`elem` variables act)
      bind2CellStep addr fb t
        = add (Event t) $ CADStep $ "Bind " ++ show fb ++ " to cell " ++ show addr
      updateLastWrite t cell | Target _ <- epdType = cell{ lastWrite=Just t }
                             | otherwise = cell{ lastWrite=Nothing }

      schedule addr job
        = let (p', job'@Job{..}) = scheduleWork addr job
          in if null actions
            then (finishSchedule p' job', Nothing)
            else (p', Just job')

      scheduleWork _addr Job{ actions=[] } = error "Fram:scheudle internal error."
      scheduleWork addr job@Job{ actions=x:xs, .. }
        = let ((ep, instrs), p') = modifyProcess p $ do
                e <- add (Activity $ act^.at) $ EndpointStep $ act^.endType
                i1 <- addInstr pu (act^.at) $ act2Instruction addr $ act^.endType
                is <- if tick0 < act^.at.infimum
                  then do
                    i2 <- addInstr pu (tick0 ... act^.at.infimum - 1) Nop
                    return [ i1, i2 ]
                  else return [ i1 ]
                mapM_ (relation . Vertical e) instrs
                setProcessTime $ act^.at.supremum + 1
                return (e, is)
          in (p', job{ endpoints=ep : endpoints
                     , instructions=instrs ++ instructions
                     , startAt=startAt `orElse` Just (act^.at.infimum)
                     , actions=if x == act^.endType then xs else (x \\\ (act^.endType)) : xs
                     })
      finishSchedule p' Job{..} = snd $ modifyProcess p' $ do
        let start = fromMaybe (error "startAt field is empty!") startAt
        h <- add (Activity $ start ... act^.at.supremum) $ FBStep $ fromFSet functionalBlock
        mapM_ (relation . Vertical h) cads
        mapM_ (relation . Vertical h) endpoints
        mapM_ (relation . Vertical h) instructions

      act2Instruction addr (Source _) = Load addr
      act2Instruction addr (Target _) = Save addr



cellEndpoints _blockAllow Cell{ input=Def Job{ actions=x:_ } }    = Right x
cellEndpoints _blockAllow Cell{ current=Just Job{ actions=x:_ } } = Right x
cellEndpoints True        Cell{ output=Def Job{actions=x:_ } }    = Right x
cellEndpoints _ _                                                 = Left undefined



findCell Fram{..} fb@(Reg' _)
  | let cs = filter ( isRight . bindToCell [] fb . snd ) $ assocs frMemory
  , not $ null cs
  = Right $ minimumOn cellLoad cs
findCell fr (Loop' _)     = findFreeCell fr
findCell fr (Constant' _) = findFreeCell fr
findCell _ _               = Left "Not found."

findFreeCell Fram{..}
  | let cs = filter (\(_, c) -> case c of
                                  Cell{ input=Undef, current=Nothing, output=Undef } -> True;
                                  _ -> False
                    ) $ assocs frMemory
  , not $ null cs
  = Right $ minimumOn cellLoad cs
findFreeCell _ = Left "Not found."

cellLoad (_addr, Cell{..}) = sum [ if input == UsedOrBlocked then -2 else 0
                                 , if output == Undef then -1 else 0
                                 ] :: Int



---------------------------------------------------------------------


instance ( Var v, Time t ) => Controllable (Fram v t) where

  data Microcode (Fram v t)
    = Microcode{ oeSignal :: Bool
               , wrSignal :: Bool
               , addrSignal :: Maybe Int
               }
    deriving (Show, Eq, Ord)

  data Instruction (Fram v t)
    = Nop
    | Load Int
    | Save Int
    deriving (Show)

instance Connected (Fram v t) i where
  data Link (Fram v t) i
    = Link { oe, wr :: i, addr :: [i] } deriving ( Show )
  transmitToLink Microcode{..} Link{..}
    = [ (oe, B oeSignal)
      , (wr, B wrSignal)
      ] ++ addrs
    where
      addrs = map (\(linkId, i) -> ( linkId
                                   , maybe X B $ fmap (`testBit` i) addrSignal
                                   )
                  ) $ zip (reverse addr) [0..]


instance Default (Instruction (Fram v t)) where
  def = Nop

getAddr (Load addr) = Just addr
getAddr (Save addr) = Just addr
getAddr _           = Nothing


instance UnambiguouslyDecode (Fram v t) where
  decodeInstruction  Nop        = Microcode False False Nothing
  decodeInstruction (Load addr) = Microcode True False $ Just addr
  decodeInstruction (Save addr) = Microcode False True $ Just addr



instance ( Var v, Time t
         , ProcessUnit (Fram v t) v t
         ) => Simulatable (Fram v t) v Int where
  simulateOn cntx@Cntx{..} pu@Fram{..} (FB fb)
    | Just (Constant x (O k) :: Constant (Parcel v)) <- cast fb = set cntx k x
    | Just (Loop (O k1@(k:_)) (I _k2) :: Loop (Parcel v)) <- cast fb = do
      let v = fromMaybe (addr2value $ findAddress k pu) $ cntx `get` k
      set cntx k1 v
    | Just (fb' :: Reg (Parcel v)) <- cast fb = simulate cntx fb'
    | Just (FramInput addr (O k) :: FramInput (Parcel v)) <- cast fb = do
      let v = fromMaybe (addr2value addr) $ cntx `get` head k
      set cntx k v
    | Just (FramOutput addr (I k) :: FramOutput (Parcel v)) <- cast fb = do
      v <- get cntx k
      let cntxFram' = M.alter (Just . maybe [v] (v:)) (addr, k) cntxFram
      return cntx{ cntxFram=cntxFram' }
    | otherwise = error $ "Can't simulate " ++ show fb ++ " on Fram."
    where
      addr2value addr = 0x1000 + addr -- must be coordinated with test bench initialization



---------------------------------------------------

instance ( Var v, Time t ) => TestBench (Fram v t) v Int where
  testEnviroment cntx0 pu@Fram{ frProcess=Process{..}, .. }
    = Immidiate (moduleName pu ++ "_tb.v") testBenchImp
    where
      Just cntx = foldl ( \(Just cntx') fb -> simulateOn cntx' pu fb ) (Just cntx0) $ functionalBlocks pu
      testBenchImp = renderST
        [ "module $moduleName$_tb();                                                                                 "
        , "parameter DATA_WIDTH = 32;                                                                                "
        , "parameter ATTR_WIDTH = 4;                                                                                 "
        , "                                                                                                          "
        , "/*                                                                                                        "
        , show cntx
        , show $ functionalBlocks pu
        , "*/                                                                                                        "
        , "                                                                                                          "
        , "reg clk, rst, wr, oe;                                                                                     "
        , "reg [3:0] addr;                                                                                           "
        , "reg [DATA_WIDTH-1:0]  data_in;                                                                            "
        , "reg [ATTR_WIDTH-1:0]  attr_in;                                                                            "
        , "wire [DATA_WIDTH-1:0] data_out;                                                                           "
        , "wire [ATTR_WIDTH-1:0] attr_out;                                                                           "
        , "                                                                                                          "
        , hardwareInstance pu "fram"
            NetworkLink{ clk=Name "clk"
                       , rst=Name "rst"
                       , dataWidth=Name "32"
                       , attrWidth=Name "4"
                       , dataIn=Name "data_in"
                       , attrIn=Name "attr_in"
                       , dataOut=Name "data_out"
                       , attrOut=Name "attr_out"
                       , controlBus=id
                       , cycleStart=Name "cycle"
                       }
            Link{ oe=Name "oe"
                , wr=Name "wr"
                , addr=[Name "addr"]
                }
        , "                                                                                                          "
        , verilogWorkInitialze
        , verilogClockGenerator
        , "                                                                                                          "
        , "initial                                                                                                   "
        , "  begin                                                                                                   "
        , "    \\$dumpfile(\"$moduleName$_tb.vcd\");                                                                 "
        , "    \\$dumpvars(0, $moduleName$_tb);                                                                      "
        , "    @(negedge rst);                                                                                       "
        , "    forever @(posedge clk);                                                                               "
        , "  end                                                                                                     "
        , "                                                                                                          "
        , initialFinish $ controlSignals pu
        , initialFinish $ testDataInput pu cntx
        , initialFinish $ testDataOutput pu cntx
        , "                                                                                                          "
        , "endmodule                                                                                                 "
        ]
        [ ("moduleName", moduleName pu)
        ]

controlSignals pu@Fram{ frProcess=Process{..}, ..}
  = concatMap ( ("      " ++) . (++ " @(negedge clk)\n") . showMicrocode . microcodeAt pu) [ 0 .. nextTick + 1 ]
  where
    showMicrocode Microcode{..} = concat
      [ "oe <= 'b", bool2binstr oeSignal, "; "
      , "wr <= 'b", bool2binstr wrSignal, "; "
      , "addr <= ", maybe "0" show addrSignal, "; "
      ]

testDataInput pu@Fram{ frProcess=p@Process{..}, ..} cntx
  = concatMap ( ("      " ++) . (++ " @(negedge clk);\n") . busState ) [ 0 .. nextTick + 1 ]
  where
    busState t
      | Just (Target v) <- endpointAt t p
       = "data_in <= " ++ show (fromMaybe (error ("input" ++ show v ++ show (functionalBlocks pu)) ) $ get cntx v) ++ ";"
      | otherwise = "/* NO INPUT */"

testDataOutput pu@Fram{ frProcess=p@Process{..}, ..} cntx
  = concatMap ( ("      @(posedge clk); #1; " ++) . (++ "\n") . busState ) [ 0 .. nextTick + 1 ] ++ bankCheck
  where
    busState t
      | Just (Source (v : _)) <- endpointAt t p
      = checkBus v $ maybe (error $ show ("checkBus" ++ show v ++ show cntx) ) show (get cntx v)
      | otherwise
      = "/* NO OUTPUT */"

    checkBus v value = concat
      [ "if ( !( data_out === " ++ value ++ " ) ) "
      ,   "\\$display("
      ,     "\""
      ,       "FAIL wrong value of " ++ show' v ++ " on the bus! "
      ,       "(got: %h expect: %h)"
      ,     "\", "
      ,     "data_out, " ++ value
      ,   ");"
      ]

    bankCheck
      = "\n      @(posedge clk);\n"
      ++ unlines [ "  " ++ checkBank addr v (maybe (error $ show ("bank" ++ show v ++ show cntx) ) show (get cntx v))
                 | Step{ sDesc=FBStep fb, .. } <- filter (isFB . sDesc) steps
                 , let addr_v = outputStep fb
                 , isJust addr_v
                 , let Just (addr, v) = addr_v
                 ]

    outputStep (FB fb)
      | Just (Loop _bs (I a)) <- cast fb = Just (findAddress a pu, a)
      | Just (FramOutput addr (I a)) <- cast fb = Just (addr, a)
      | otherwise = Nothing

    checkBank addr v value = concatMap ("    " ++)
      [ "if ( !( fram.bank[" ++ show addr ++ "] === " ++ show value ++ " ) ) "
      ,   "\\$display("
      ,     "\""
      ,       "FAIL wrong value of " ++ show' v ++ " in fram bank[" ++ show' addr ++ "]! "
      ,       "(got: %h expect: %h)"
      ,     "\","
      ,     "data_out, " ++ show value
      ,   ");"
      ]
    show' s = filter (/= '\"') $ show s



findAddress var pu@Fram{ frProcess=p@Process{..} }
  | [ time ] <- variableSendAt var
  , [ instr ] <- mapMaybe (extractInstruction pu >=> getAddr) $ whatsHappen (time^.infimum) p
  = instr
  | otherwise = error $ "Can't find instruction for effect of variable: " ++ show var
  where
    variableSendAt v = [ t | Step{ sTime=Activity t
                                 , sDesc=EndpointStep endpoints
                                 } <- steps
                           , v `elem` variables endpoints
                           ]


instance ( Time t, Var v ) => DefinitionSynthesis (Fram v t) where
  moduleName _ = "pu_fram"
  hardware pu = FromLibrary $ moduleName pu ++ ".v"
  software _ = Empty

instance ( Time t, Var v
         ) => Synthesis (Fram v t) LinkId where
  hardwareInstance Fram{..} name NetworkLink{..} Link{..} = renderST
    [ "pu_fram "
    , "  #( .DATA_WIDTH( " ++ link dataWidth ++ " )"
    , "   , .ATTR_WIDTH( " ++ link attrWidth ++ " )"
    , "   , .RAM_SIZE( " ++ show frSize ++ " )"
    , "   ) " ++ name
    , "  ( .clk( " ++ link clk ++ " )"
    , "  , .signal_addr( { " ++ S.join ", " (map control addr) ++ " } )"
    , ""
    , "  , .signal_wr( " ++ control wr ++ " )"
    , "  , .data_in( " ++ link dataIn ++ " )"
    , "  , .attr_in( " ++ link attrIn ++ " )"
    , ""
    , "  , .signal_oe( " ++ control oe ++ " )"
    , "  , .data_out( " ++ link dataOut ++ " )"
    , "  , .attr_out( " ++ link attrOut ++ " )"
    , "  );"
    , "initial begin"
    , S.join "\n"
        $ map (\(i, Cell{..}) -> "  $name$.bank[" ++ show i ++ "] <= " ++ show initialValue ++ ";")
        $ assocs frMemory
    , "end"
    ] [ ("name", name)
      , ("size", show frSize)
      ]
    where
      control = link . controlBus
