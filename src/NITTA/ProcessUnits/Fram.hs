{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
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
  , Signal(..)
  )
where

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
import           NITTA.Compiler
import           NITTA.FunctionBlocks
import           NITTA.Lens
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval      ((...))
import           Prelude               hiding (last)



data Fram v t = Fram
  { frMemory   :: Array Int (Cell v t)
  -- | Информация о функциональных блоках, которые необходимо обработать fram-у. Требуют хранения
  -- дополнительной информации, такой как время привязки функционального блока. Нельзя сразу делать
  -- привязку к ячейке памяти, так как это будет неэффективно.
  , frRemains  :: [ (FSet (Fram v t), ProcessUid) ]
  , frBindedFB :: [ FB Parcel v ]
  , frProcess  :: Process v t
  , frSize     :: Int
  } deriving ( Show )

instance ( Default t ) => Default (Fram v t) where
  def = Fram { frMemory=listArray (0, defaultSize - 1) $ cells
             , frBindedFB=[]
             , frRemains=[]
             , frProcess=def
             , frSize=defaultSize
             }
    where
      defaultSize = 16
      cells = map (\(i, c) -> c{ initialValue=0x1000 + i }) $ zip [0..] $ repeat def

instance WithFunctionalBlocks (Fram v t) (FB Parcel v) where
  functionalBlocks Fram{..} = frBindedFB



instance FunctionalSet (Fram v t) where
  data FSet (Fram v t)
    = FramInput' (FramInput Parcel v)
    | FramOutput' (FramOutput Parcel v)
    | Loop' (Loop Parcel v)
    | Reg' (Reg Parcel v)
    | Constant' (Constant Parcel v)
    deriving ( Show, Eq )

instance ( Var v ) => WithFunctionalBlocks (FSet (Fram v t)) (FB Parcel v) where
  -- TODO: Сделать данную операцию через Generics.
  functionalBlocks (FramInput' fb)  = [ boxFB fb ]
  functionalBlocks (FramOutput' fb) = [ boxFB fb ]
  functionalBlocks (Loop' fb)       = [ boxFB fb ]
  functionalBlocks (Reg' fb)        = [ boxFB fb ]
  functionalBlocks (Constant' fb)   = [ boxFB fb ]

instance ( Var v ) => ToFSet (Fram v t) v where
  toFSet fb0
    | Just fb@(Constant _ _) <- castFB fb0 = Right $ Constant' fb
    | Just fb@(Reg _ _) <- castFB fb0 = Right $ Reg' fb
    | Just fb@(Loop _ _) <- castFB fb0 = Right $ Loop' fb
    | Just fb@(FramInput _ _) <- castFB fb0 = Right $ FramInput' fb
    | Just fb@(FramOutput _ _) <- castFB fb0 = Right $ FramOutput' fb
    | otherwise = Left $ "Fram don't support " ++ show fb0

isReg (Reg' _) = True
isReg _        = False



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



instance ( IOType Parcel v, Var v, Time t, WithFunctionalBlocks (Fram v t) (FB Parcel v) ) => ProcessUnit (Fram v t) v t where
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

  options _proxy fr@Fram{ frProcess=Process{..}, ..} = fromCells ++ fromRemain
    where
      fromRemain = [ EndpointO ep $ constrain c ep
                   | (fb, cad) <- frRemains
                   , (c, ep) <- toList $ do
                       (_addr, cell) <- findCell fr fb
                       cell' <- bindToCell [cad] fb cell
                       ep <- cellEndpoints False cell'
                       return (cell', ep)
                   ]

      fromCells = [ EndpointO ep $ constrain cell ep
                  | (_addr, cell@Cell{..}) <- assocs frMemory
                  , ep <- toList $ cellEndpoints isTargetAllow cell
                  ]

      -- | Загрузка в память значения на следующий вычислительный цикл не позволяет использовать её
      -- в качестве регистра на текущем цикле.
      isTargetAllow = null (filter (isReg . fst) frRemains) && (frSize - numberOfCellForReg > 1)
      -- | Количество ячеек, которые могут быть использованы для Reg.
      numberOfCellForReg = length $ filter (\Cell{..} -> output == UsedOrBlocked) $ elems frMemory

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



cellEndpoints _allowOutput Cell{ input=Def Job{ actions=x:_ } }    = Right x
cellEndpoints _allowOutput Cell{ current=Just Job{ actions=x:_ } } = Right x
cellEndpoints True         Cell{ output=Def Job{actions=x:_ } }    = Right x
cellEndpoints _ _                                                  = Left undefined



findCell Fram{..} fb@(Reg' _)
  = let cs = filter ( isRight . bindToCell [] fb . snd ) $ assocs frMemory
    in Right $ minimumOn cellLoad cs
findCell fr (Loop' _)     = findFreeCell fr
findCell fr (Constant' _) = findFreeCell fr
findCell _ _               = Left "Not found."

findFreeCell Fram{..}
  = let cs = filter (\(_, c) -> case c of
                                  Cell{ input=Undef, current=Nothing, output=Undef } -> True;
                                  _ -> False
                    ) $ assocs frMemory
    in Right $ minimumOn cellLoad cs

cellLoad (_addr, Cell{..}) = sum [ if input == UsedOrBlocked then -2 else 0
                                 , if output == Undef then -1 else 0
                                 ] :: Int



---------------------------------------------------------------------


instance ( Var v, Time t ) => Controllable (Fram v t) where

  data Signal (Fram v t)
    = OE
    | WR
    | ADDR Int
    deriving (Show, Eq, Ord)

  data Instruction (Fram v t)
    = Nop
    | Load Int
    | Save Int
    deriving (Show)

instance Default (Instruction (Fram v t)) where
  def = Nop

getAddr (Load addr) = Just addr
getAddr (Save addr) = Just addr
getAddr _           = Nothing

instance ( Var v, Time t
         ) => ByTime (Fram v t) t where
  signalAt pu@Fram{..} time sig =
    let instruction = case mapMaybe (extractInstruction pu) $ whatsHappen time frProcess of
          []  -> Nop
          [i] -> i
          is  -> error $ "Ambiguously instruction at "
                       ++ show time ++ ": " ++ show is
    in decodeInstruction instruction sig


instance UnambiguouslyDecode (Fram v t) where
  decodeInstruction  Nop        (ADDR _) = X
  decodeInstruction  Nop         _       = B False
  decodeInstruction (Load addr) (ADDR b) = B $ testBit addr b
  decodeInstruction (Load    _)  OE      = B True
  decodeInstruction (Load    _)  WR      = B False
  decodeInstruction (Save addr) (ADDR b) = B $ testBit addr b
  decodeInstruction (Save    _)  OE      = B False
  decodeInstruction (Save    _)  WR      = B True



instance ( Var v, Time t
         , ProcessUnit (Fram v t) v t
         ) => Simulatable (Fram v t) v Int where
  variableValue fb pu@Fram{..} cntx (v, i) = either error id $ do
    fbs <- toFSet fb
    return $ variableValue' fbs
    where
      addr2value addr = 0x1000 + addr -- must be coordinated with test bench initialization
      variableValue' (Constant' (Constant x (O b))) | v `elem` b = x
      variableValue' (Loop' (Loop _bs (I a))) | a == v = cntx M.! (v, i)
      variableValue' (Loop' (Loop (O bs) _a)) | v `elem` bs, i == 0 = addr2value $ findAddress v pu
      variableValue' (Reg' (Reg (I a) _bs)) | a == v = cntx M.! (v, i)
      variableValue' (Reg' (Reg (I a) (O bs))) | v `elem` bs = cntx M.! (a, i)

      variableValue' (FramInput' (FramInput addr (O bs))) | i == 0, v `elem` bs = addr2value addr
      variableValue' (FramOutput' (FramOutput _addr (I a))) | v == a = cntx M.! (v, i)
      variableValue' _ = error $ "Fram can't simulate " ++ show (v, i)
                              ++ " for " ++ show fb
                              ++ " in contex " ++ show cntx



---------------------------------------------------

instance TestBenchRun (Fram v t) where
  buildArgs _ = [ "hdl/pu_fram.v"
                , "hdl/pu_fram_tb.v"
                ]

instance ( Var v, Time t ) => TestBench (Fram v t) v Int where

  components _ =
    [ ( "hdl/gen/pu_fram_inputs.v", testInputs )
    , ( "hdl/gen/pu_fram_signals.v", testSignals )
    , ( "hdl/gen/pu_fram_outputs.v", testOutputs )
    ]

  simulateContext fr@Fram{ frProcess=p@Process{..}, .. } cntx =
    let vs = [ v | eff <- getEndpoints p
                 , v <- variables eff
                 ]
    in foldl ( \cntx' v ->
                 M.insert (v, 0)
                          (variableValueWithoutFB fr cntx' (v, 0))
                          cntx'
             ) cntx vs



testSignals fram@Fram{ frProcess=Process{..}, ..} _cntx
  = concatMap ( (++ " @(negedge clk)\n") . showSignals . signalsAt ) [ 0 .. nextTick + 1 ]
  where
    signalsAt time = map (signalAt fram time)
                     [ OE, WR, ADDR 3, ADDR 2, ADDR 1, ADDR 0 ]
    showSignals = (\[oe, wr, a3, a2, a1, a0] ->
                      "oe <= 'b" ++ oe
                      ++ "; wr <= 'b" ++ wr
                      ++ "; addr[3] <= 'b" ++ a3
                      ++ "; addr[2] <= 'b" ++ a2
                      ++ "; addr[1] <= 'b" ++ a1
                      ++ "; addr[0] <= 'b" ++ a0 ++ ";"
                  ) . map show



testInputs Fram{ frProcess=p@Process{..}, ..} cntx
  = concatMap ( (++ " @(negedge clk);\n") . busState ) [ 0 .. nextTick + 1 ]
  where
    busState t
      | Just (Target v) <- endpointAt t p = "value_i <= " ++ show (cntx M.! (v, 0)) ++ ";"
      | otherwise = "/* NO INPUT */"

testOutputs pu@Fram{ frProcess=p@Process{..}, ..} cntx
  = concatMap ( ("@(posedge clk); #1; " ++) . (++ "\n") . busState ) [ 0 .. nextTick + 1 ] ++ bankCheck
  where
    busState t
      | Just (Source (v : _)) <- endpointAt t p
      = checkBus v $ cntx M.! (v, 0)
      | otherwise
      = "/* NO OUTPUT */"

    checkBus v value = concat
      [ "if ( !( value_o == " ++ show value ++ " ) ) "
      ,   "$display("
      ,     "\""
      ,       "FAIL wrong value of " ++ show' v ++ " on the bus! "
      ,       "(got: %h expect: %h)"
      ,     "\","
      ,     "value_o, " ++ show value
      ,   ");"
      ]

    bankCheck = "\n\n@(posedge clk);\n"
      ++ concat [ checkBank addr v (cntx M.! (v, 0))
                | Step{ sDesc=FBStep fb, .. } <- filter (isFB . sDesc) steps
                , let addr_v = outputStep fb
                , isJust addr_v
                , let Just (addr, v) = addr_v
                ]

    outputStep fb
      | Just (Loop _bs (I a)) <- castFB fb = Just (findAddress a pu, a)
      | Just (FramOutput addr (I a)) <- castFB fb = Just (addr, a)
      | otherwise = Nothing

    checkBank addr v value = concat
      [ "if ( !( fram.bank[" ++ show addr ++ "] == " ++ show value ++ " ) ) "
      ,   "$display("
      ,     "\""
      ,       "FAIL wrong value of " ++ show' v ++ " in fram bank[" ++ show' addr ++ "]! "
      ,       "(got: %h expect: %h)"
      ,     "\","
      ,     "value_o, " ++ show value
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


instance ( Time t, Var v ) => Synthesis (Fram v t) where
  moduleInstance Fram{..} name cntx
    = renderST
      [ "pu_fram "
      , "    #( .RAM_SIZE( $size$ )"
      , "     ) $name$ ("
      , "    .clk( $Clk$ ),"
      , "    .signal_addr( { $ADDR_3$, $ADDR_2$, $ADDR_1$, $ADDR_0$ } ),"
      , ""
      , "    .signal_wr( $WR$ ),"
      , "    .data_in( $DataIn$ ),"
      , "    .attr_in( $AttrIn$ ),"
      , ""
      , "    .signal_oe( $OE$ ),"
      , "    .data_out( $DataOut$ ),"
      , "    .attr_out( $AttrOut$ ) "
      , ");"
      , "initial begin"
      , S.join "\n"
          $ map (\(i, Cell{..}) -> "    $name$.bank[" ++ show i ++ "] <= " ++ show initialValue ++ ";")
          $ assocs frMemory
      , "end"
      ] $ ("name", name) : ("size", show frSize) : cntx
  moduleName _ = "pu_fram"
  moduleDefinition = undefined
