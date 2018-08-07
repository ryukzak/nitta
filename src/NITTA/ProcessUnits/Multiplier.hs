{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-|
Module : Multiplier (модель вычислительного блока умножителя)

В данном модуле реализуется модель вычислительного блока умножителя для САПР.

Вычислительный блок может вычислять следующие функциональные блоки:

- 'NITTA.FunctionBlocks.Multiply'.

В один момент времени может вычислиться только один функциональный блок, при этом его выполнение не
может быть прервано.

Данный модуль следует рассматривать как образец при реализации других моделей вычислительных блоков.
Его исходный код написан в «литературном стиле», в связи с чем рекомендуем продолжить чтение
документации прямо в исходном коде.

= Пример работы

Рассмотрим пример ручного планирования вычислительного процесса для одного функционального блока.
Для воспроизведения следует запустить 'stack repl'. Высока вероятность, что с актуальной версией ПО
вывод может несколько отличаться.

Подключаем необходимые модули и настраиваем приглашение консоли.

>>> :l NITTA.ProcessUnits.Multiplier
[ 1 of 10] Compiling NITTA.Types.Poly ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Poly.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Poly.o ) [flags changed]
[ 2 of 10] Compiling NITTA.Types.Time ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Time.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Time.o ) [flags changed]
[ 3 of 10] Compiling NITTA.Types.Base ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Base.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Base.o ) [flags changed]
[ 4 of 10] Compiling NITTA.Types.Network ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Network.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Network.o ) [flags changed]
[ 5 of 10] Compiling NITTA.Types      ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types.o ) [flags changed]
[ 6 of 10] Compiling NITTA.Utils.Lens ( /Users/penskoi/Documents/src/nitta/src/NITTA/Utils/Lens.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Utils/Lens.o ) [flags changed]
[ 7 of 10] Compiling NITTA.Utils      ( /Users/penskoi/Documents/src/nitta/src/NITTA/Utils.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Utils.o ) [flags changed]
[ 8 of 10] Compiling NITTA.FunctionBlocks ( /Users/penskoi/Documents/src/nitta/src/NITTA/FunctionBlocks.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/FunctionBlocks.o ) [flags changed]
[10 of 10] Compiling NITTA.ProcessUnits.Multiplier ( /Users/penskoi/Documents/src/nitta/src/NITTA/ProcessUnits/Multiplier.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/ProcessUnits/Multiplier.o )
Ok, 10 modules loaded.
>>> :module +NITTA.Types NITTA.FunctionBlocks Numeric.Interval Data.Set
>>> :set prompt "\ESC[34mλ> \ESC[m"

Создаём функциональный блок и начальное состояние модели вычислительного блока умножителя. К
сожалению, Haskell-у не хватает информации из контекста, что бы до конца вывести их типы,
по этому мы зададим их вручную.

>>> fb = multiply "a" "b" ["c", "d"] :: FB (Parcel String Int)
>>> fb
<Multiply (I "a") (I "b") (O (fromList ["c","d"]))>
>>> st0 = multiplier True :: Multiplier String Int Int
>>> st0
Multiplier {puRemain = [], puTarget = [], puSource = [], puDoneAt = Nothing, puProcess = Process {steps = [], relations = [], nextTick = 0, nextUid = 0}, puMocked = True}
>>> options endpointDT st0
[]

Назначим функциональный блок вычислительному блоку. Данная операция может выполняться в любой момент
работы с вычислительным блоком.

>>> Right st1 = tryBind fb st0
>>> st1
Multiplier {puRemain = [<Multiply (I "a") (I "b") (O (fromList ["c","d"]))>], puTarget = [], puSource = [], puDoneAt = Nothing, puProcess = Process {steps = [], relations = [], nextTick = 0, nextUid = 0}, puMocked = True}
>>> mapM_ print $ options endpointDT st1
?Target "a"@(0..∞ /P 1..∞)
?Target "b"@(0..∞ /P 1..∞)

Как можно видеть, после назначения у нас появилось два варианта развития вычислительного процесса,
соответствующих разным последовательностям загрузки аргументов. Спланируем процесс хагрузки
аргументов.

>>> st2 = decision endpointDT st1 $ EndpointD (Target "a") (0...2)
>>> st2
Multiplier {puRemain = [], puTarget = ["b"], puSource = ["c","d"], puDoneAt = Nothing, puProcess = Process {steps = [Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 3, nextUid = 2}, puMocked = True}
>>> mapM_ print $ options endpointDT st2
?Target "b"@(3..∞ /P 1..∞)
>>> st3 = decision endpointDT st2 $ EndpointD (Target "b") (3...3)
>>> st3
Multiplier {puRemain = [], puTarget = [], puSource = ["c","d"], puDoneAt = Just 6, puProcess = Process {steps = [Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 4, nextUid = 4}, puMocked = True}
>>> mapM_ print $ options endpointDT st3
?Source (fromList ["c","d"])@(6..∞ /P 1..∞)

После загрузки всех аргуметом можно видеть, что следующим вариантом является выгрузка переменных "c"
и "d" из вычислительного блока умножителя. Важно отметить, что переменные могут выгружаться как
параллельно, так и последовательно. Далее будет рассмотрен второй вариант.

>>> st4 = decision endpointDT st3 $ EndpointD (Source $ fromList ["c"]) (6...6)
>>> st4
Multiplier {puRemain = [], puTarget = [], puSource = ["d"], puDoneAt = Just 6, puProcess = Process {steps = [Step {sKey = 5, sTime = Activity (6 ... 6), sDesc = Out},Step {sKey = 4, sTime = Activity (6 ... 6), sDesc = Source (fromList ["c"])},Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 7, nextUid = 6}, puMocked = True}
>>> mapM_ print $ options endpointDT st4
?Source (fromList ["d"])@(7..∞ /P 1..∞)
>>> st5 = decision endpointDT st4 $ EndpointD (Source $ fromList ["d"]) (7...7)
>>> st5
Multiplier {puRemain = [], puTarget = [], puSource = [], puDoneAt = Nothing, puProcess = Process {steps = [Step {sKey = 7, sTime = Activity (7 ... 7), sDesc = Out},Step {sKey = 6, sTime = Activity (7 ... 7), sDesc = Source (fromList ["d"])},Step {sKey = 5, sTime = Activity (6 ... 6), sDesc = Out},Step {sKey = 4, sTime = Activity (6 ... 6), sDesc = Source (fromList ["c"])},Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 8, nextUid = 8}, puMocked = True}
>>> options endpointDT st5
[]

Варианты развития вычислительного процесса закончились. Все назначеные функциональные блоки выполнены.
-}

-- TODO: Перспективным направлением по развитию данного вычислительного блока является внедрение в
-- него регистра накопителя, что позволит перемножать произвольное количество аргументов, что
-- сократит количество транзакций по шине данных при перемножении более двух переменных.

module NITTA.ProcessUnits.Multiplier
    ( multiplier
    , Multiplier
    , PUPorts(..)
    ) where

import           Data.Default
import           Data.List            (find, partition, (\\))
import           Data.Set             (elems, fromList, member)
import           Data.Typeable
import           NITTA.FunctionBlocks (castFB)
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Process
import           Numeric.Interval     (inf, sup, (...))

{-
* Вычислительный блок

Вычислительные блоки могут реализовывать:

- хранение и обработку данных;
- взаимодействие с периферией;
- управление и контроль за вычислителем.

При этом, они характеризуются сложным поведением, выраженном в:

- многофункциональности;
- внутреннем параллелизме;
- суперскалярности;
- конвейеризации;
- наличие внутренних ресурсов;
- специализацией интерфейса управления.

Поведение вычислительного блока определяется прикладным алгоритмом, являющимся композицией
функциональных блоков ('NITTA.FunctionBlocks'). Функциональные блоки привязываются САПР к конкретным
вычислительным блокам, а те, в свою очередь, реализуют требуемый процесс.

Любой вычислительный блок подразумевает три составляющие:

- аппаратное обеспечение вычислительного блока - набор заранее подготовленных либо автоматически
  генерируемых файлов описания аппаратруры на Hardware Description Language;
- программное обеспечение вычислительного блока - набор бинарных файлов задающих:
  - начальное состояние и настройки вычислительного блока;
  - управляющую программу;
- модель вычислительного блока в САПР - компонент САПР, осуществляющий поддержку вычислительного
  блока (генерация аппаратной и программной состоявляющей, объекдинение вычилсительных блоков в
  процессора, планирование вычислительного процесса и т.д.).

При этом все три составляющие являются сильно связанными между собой и должны строго друг-другу
соответствовать. Для глубокого понимания принципов функционирования вычислительного блока необходимо
иметь представление обо всех его частях. Ниже будет подробно рассмотрена модель вычислительного
блока умножителя и то, как она реализована.
-}

{-
* Модель вычислительного блока

Целю модели вычислительного блока является «научить» САПР работать с вычислительным блоком, а
именно:

- какие функциональные блоки могут быть вычислены с его помощью (см. 'NITTA.Type.ProcessUnit');
- назначить экземпляру вычислительного блока выполнение указанного функционального блока (см.
  'NITTA.Type.ProcessUnit');
- система команд вычислительного блока и структура микрокода, позволяющая им управлять (см.
  'NITTA.Type.Controllable');
- отобразить однозначно трактуемые инструкции в микрокод (см. 'NITTA.Type. UnambiguouslyDecode');
- какие есть варианты ('options') развития вычислительного процесса (загрузить или выгрузить ту или
  иную переменную или переменные);
- спланировать вычислительный процесс, описываемый загрузкой или выгрузкой переменных в или из
  вычислительного блока (см. 'decision');
-}

{-|
Основой модели вычислительного блока является структура данных, фиксирующая:

- состояние вычислительного блока на протяжении всего процесса планирования вычислительного
  процесса;
- описание вычислительного процесса (целиком или фрагмента), которе может быть транслировано в
  программное обеспечение.

Именно вокруг данной структуры данных и строится вся алгоритмическая часть модели вычислительного
блока. Структура данных параметризуется следующими переменными типа:

- v - идентификатор перемменой;
- x - тип значений, с которыми работает умножитель;
- t - идентификатор момента времени.
-}
data Multiplier v x t
    -- TODO: Перенести время из процесса сюда.
    
    -- TODO: Сделать реализацию безопастной (выкидывать ошибку при попытке спланировать не корректный ВП).
    = Multiplier
        { -- |Список назначенных, но еще необработанных или необрабатываемых функциональных блоков.
          -- Выполнение функционального блока начинается с:
          --
          -- 1) удаления функционального блока из данного списка;
          -- 2) переноса информации из функционального блока в поля `puTarget` и `puSource`
          --    (реализуемого функцией 'assignment').
          --
          -- Функциональные блоки могут выполняться в произвольном порядке. Хранение информации о
          -- выполненных функциональных блоках не осуществляется, так как она есть в описание
          -- вычислительного процесса `puProcess`.
          puRemain  :: [FB (Parcel v x)]
          -- |Список переменных, которые необходимо загрузить в вычислительный блок.
        , puTarget  :: [v]
          -- |Список переменных, которые необходимо выгрузить из вычислительного блока в любом
          -- порядке. Важно отметить, что все выгружаемые переменные соответствуют одному значению -
          -- результату умножения.
        , puSource  :: [v]
          -- |Процесс умножения будет завершён в указанный момент времени. Значение устанавливается
          -- сразу после загрузки аргументов. Необходимо что бы помнить, когда умножение было
          -- завершено на самом деле.
        , puDoneAt  :: Maybe t
          -- |Описание вычислительного процесса, спланированного для данного вычислительного блока.
        , puProcess :: Process (Parcel v x) t
          -- |В реализации данного вычислительного блока используется IP ядро поставляемое вместе с
          -- Altera Quartus, что не позволяет осуществлять симуляцию при помощи Icarus Verilog.
          -- Чтобы обойти эту проблему была создана заглушка, подключаемая вместо IP ядра если
          -- установлен данный флаг.
        , puMocked  :: Bool
        }
    deriving ( Show )


-- |Конструктор модели умножителей вычислительного блока. Аргумент определяет внутреннюю структуру
-- аппаратуры вычислительного блока: использование IP ядра умножителя (False) или заглушки (True).
multiplier mock = Multiplier [] [] [] Nothing def mock



-- |Привязку функциональных блоков к вычислительным осуществляет данный класс типов. Он осуществляет
-- проверку может ли функциональный блок быть вычисленным и если да - осуществляет эту привязку. При
-- этом отказ в привязке может быть связан как с тем, что тип функционального блока не
-- поддерживается, так и с тем что исчерпаны внутрении ресурсы вычислительного блока.
--
-- С точки зрения САПР привязка выглядит следующим образом: САПР опрашивает модели всех
-- наличиствующих экземпляров вычислительных блоков и получает список тех, которые готовы взять в
-- работу рассматриваемый функциональный блок. Затем, на основании различных метрик (как например
-- загрузка вычислительных блоков, количество и тип ещё не привязанных функциональных блоков)
-- выбирается лучший вариант.
instance ( Var v, Time t
         ) => ProcessUnit (Multiplier v x t) (Parcel v x) t where
    -- |Привязка вычислительного блока осуществялется этой функцией.
    tryBind fb pu@Multiplier{ puRemain }
        -- Для этого осуществляется проверка, приводится ли тип функционалнього блока к одному из
        -- поддерживаемых ('castFB') и в случае успеха возвращается состояние модели после привязки
        -- с меткой 'Right'.
        --
        -- Важно отметить, что "привязка" не означается фактическое начало работы над функциональным
        -- блоком, что позволяет сперва осуществить привязку всех задач, а уже потом планировать
        -- вычислительный процесс.
        | Just FB.Multiply{} <- castFB fb = Right pu{ puRemain=fb : puRemain }
        -- В случае невозможности привязки возвращается строка с кратким описание причины отказа и
        -- меткой 'Left'.
        | otherwise = Left $ "The functional block is unsupported by Multiplier: " ++ show fb
    -- |Просто реализуется унифицированный интерфейс для получения описания вычислительного
    -- процесса.
    process = puProcess
    -- |Данный метод используется для установки времени вычислительного блока снаружи. В настоящий
    -- момент это необходимо только для реализации ветвления, которое в текущий момент не работает
    -- корректно.
    setTime t pu@Multiplier{ puProcess } = pu{ puProcess=puProcess{ nextTick=t } }


-- |Данная функция осуществляет фактическое взятие функционального блока в работу.
assignment pu@Multiplier{ puTarget=[], puSource=[], puRemain } fb
    | Just (FB.Multiply (I a) (I b) (O c)) <- castFB fb = pu{ puTarget=[a, b], puSource=elems c, puRemain=puRemain \\ [ fb ] }
assignment _ _ = error "Multiplier: internal assignment error."



{-
Результатом планирования является описание одного вычислительного цикла, которое в последствии может
быть транслировано в микрокод, непосредственно управляющий вычислительным блоком. С точки зрения
блока, процесс может быть описан как последовательное выполнение роли источника данных ('Source')
или получателя данных ('Target').

Сам же процесс планирования состоит из двух операций выполняемых в цикле:
-}
instance ( Var v, Time t, Typeable x
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Multiplier v x t)
        where
    -- 1. Опрос вычислительного блока относительно того, в каких ролях он готов выступить (другими
    --    словами, как может развиваться вычислительный процесс). Он реализуется фунцией options,
    --    результатом которой является один из следующих списков:
    --    - список вариантов загружаемых в вычислительный блок переменных, необходимых для
    --      находящегося в работе функционального блока;
    options _proxy Multiplier{ puTarget=vs@(_:_), puProcess=Process{ nextTick } }
        = map (\v -> EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (1 ... maxBound)) vs
    --    - список вариантов выгружаемых из вычислительного блока переменных;
    options _proxy Multiplier{ puSource, puDoneAt=Just at, puProcess=Process{ nextTick } } | not $ null puSource
        = [ EndpointO (Source $ fromList puSource) $ TimeConstrain (max at nextTick ... maxBound) (1 ... maxBound) ]
    --    - список вариантов загружаемых в вычислительный блок переменных, загрузка любой из которых
    --      приведёт к фактическу началу работы над соответствующим функциональным блоком.
    options proxy pu@Multiplier{ puRemain } = concatMap (options proxy . assignment pu) puRemain
    --    Отметим, что предоставляемые данной функцией варианты требут уточнений, так как:
    --    1. Указывают не конкретный момент времени для работы, а доступный интервал
    --       ('TimeConstrain' (с и по какой момент можно осуществить загрузку/выгрузку) (сколько она
    --       может длиться)).
    --    2. Одно значение может выгружаться из вычислительного блока как несколько различных
    --       переменных. Это может осуществляться как единомоментно (на уровне аппаратуры на шину
    --       выставляется значение и считывается сразу несколькими вычислительными блоками), так и
    --       последовательно (сперва значение на шину будет выставлено для одного вычислительного
    --       блока, а затем для другого), что также должно уточняться.

    -- 2. Планирование процесса. К состоянию модели вычислительного блока применяется решение
    --    относительно дальшейшего развития вычислительного процесса (преобразование варианта
    --    полученного из 'options' осуществляется САПР за пределами модели) путём вызова фунции
    --    'decision'.
    --
    --    Если модель ожидает, что в неё загрузят переменную.
    decision _proxy pu@Multiplier{ puTarget=vs } d@EndpointD{ epdRole=Target v, epdAt }
        -- Из списка загружаемых значений извлекается трубуемая переменная, а остаток - сохраняется
        -- для следующих шагов.
        | ([_], xs) <- partition (== v) vs
        -- Переменная 'sel' используется для того, что бы зафиксировать очерёдность загрузки
        -- переменных в аппаратный блок, что необходимо из-за особенностей реализации.
        , let sel = if null xs then B else A
        = pu
            { -- Осуществляется планирование вычислительного процесса.
              puProcess=schedule pu $
                scheduleEndpoint d $ scheduleInstructionAndUpdateTick (inf epdAt) (sup epdAt) $ Load sel
              -- Сохраняется остаток работы на следующий цикл.
            , puTarget=xs
              -- Если загружены все необходимые аргументы (null xs), то сохраняется момент времяни,
              -- когда будет получен результат.
            , puDoneAt=if null xs
                then Just $ sup epdAt + 3
                else Nothing
            }
    --    Если модель ожидает, что из неё выгрузят переменные. 
    decision _proxy pu@Multiplier{ puTarget=[], puSource, puDoneAt } d@EndpointD{ epdRole=Source v, epdAt }
        | not $ null puSource
        , let puSource' = puSource \\ elems v
        , puSource' /= puSource
        = pu
            { -- Осуществляется планирование вычислительного процесса.
              puProcess=schedule pu $
                scheduleEndpoint d $ scheduleInstructionAndUpdateTick (inf epdAt) (sup epdAt) Out
              -- В случае если не все переменные были запрошены - сохраняются оставшиеся.
            , puSource=puSource'
              -- Если вся работа выполнена, то сбрасывается время готовности результата.
            , puDoneAt=if null puSource' then Nothing else puDoneAt
            }
    --    Если никакой функциональный блок в настоящий момент не выполняется, значит необходимо
    --    найти в списке назначенных блоков требуемый, запустить его в работу, и только затем
    --    принять решение и спланировать фрагмент вычислительного процесса.
    decision proxy pu@Multiplier{ puTarget=[], puSource=[], puRemain } d
        | let v = oneOf $ variables d
        , Just fb <- find (\fb -> v `member` variables fb) puRemain
        = decision proxy (assignment pu fb) d
    decision _ pu d = error $ "Multiplier decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d



-- |Идентификатор аргумента операции умножения.
--
-- Как ранее говорилось, из-за особенностей аппаратной реализации, в спланированом процессе
-- необходимо учитывать последовательность загрузки операндов. Для этого и определён данный тип. При
-- этом необходимо отметить, что с точки зрения алгоритма и модели порядок аргументов не имеет
-- значения, что отражено в реализованном выше классе, отвечающем за планирование вычислительного
-- процесса.
data ArgumentSelector = A | B
    deriving ( Show, Eq )



-- |Перейдём непосредственно к вопросам организации вычислительного процесса на уровне аппаратуры.
-- Для этого на уровне модели определено два уровня представления:
--
-- - уровень инструкций, в рамках которого описывается вычислительный процесс в понятной для
--   разработчика форме;
-- - уровень микрокода, в рамках которого описывается структура управляющих вычислительным блоком
--   сигналов и их значения.
instance Controllable (Multiplier v x t) where
    -- |Инструкции, для управления вычислительным блоком умножителя. Умножитель может только
    -- загружать аргументы A и B, а также выгрузить результат умножения. Именно эти инструкции
    -- используются при планировании вычислительного процесса функцией 'schedule'.
    data Instruction (Multiplier v x t)
        = Load ArgumentSelector
        | Out
        deriving (Show)

    -- |Набор сигналов для управления вычислительным блоком и представление микрокода данного
    -- вычислительного блока.
    data Microcode (Multiplier v x t)
        = Microcode
            { -- Сигнал записи в вычислительный блок.
              wrSignal :: Bool
              -- Селектор аргумента, загружаемого в вычислительный блок.
            , selSignal :: Bool
              -- Сигнал выгрузки результата из вычислительного блока.
            , oeSignal :: Bool
            }
        deriving ( Show, Eq, Ord )

-- |Также, для микрокода необходимо определить состояние по умолчанию, которое означает, что
-- вычислительный блок находится в состоянии бездействия, при этом не занимает шину и хранит своё
-- внутренее состояние в предсказуемом виде. В случае умножителя - не сбрасывает результат
-- умножения. Состояние по умолчанию используется для остановки, паузы или ожидания вычислительного
-- блока.
instance Default (Microcode (Multiplier v x t)) where
    def = Microcode
        { wrSignal=False
        , selSignal=False
        , oeSignal=False
        }

-- |Связка инструкций и микрокода осуществляется данным классом, который требует их однозначного
-- соответствия, а также независимо от состояния и настроек модели (FIXME: не корректно для Fram и
-- заданной ширины шины адреса).
instance UnambiguouslyDecode (Multiplier v x t) where
    decodeInstruction (Load A) = def{ wrSignal=True, selSignal=False }
    decodeInstruction (Load B) = def{ wrSignal=True, selSignal=True }
    decodeInstruction Out      = def{ oeSignal=True }

-- |Определение сигнальных линий вычислительного блока, используемого для их ручного подключения к
-- сигнальной шине на уровне сети, а также функция отображения микрокода на линии. В будущем данный
-- класс будет перерабатываться с целью автоматизации данного процесса.
instance Connected (Multiplier v x t) where
    data PUPorts (Multiplier v x t)
        = PUPorts{ wr, wrSel, oe :: Signal } deriving ( Show )
    transmitToLink Microcode{..} PUPorts{..}
        =
            [ (wr, Bool wrSignal)
            , (wrSel, Bool selSignal)
            , (oe, Bool oeSignal)
            ]

-- |Ключевую роль при тестировании играет наличие эталонных значений, с которыми сравнивается
-- фактическая работы вычислительного блока в симуляторе. Генерация эталонных значений
-- осуществляется данным классом.
instance ( Var v
         , Integral x
         ) => Simulatable (Multiplier v x t) v x where
    simulateOn cntx _ fb
        | Just fb'@FB.Multiply{} <- castFB fb = simulate cntx fb'
        | otherwise = error $ "Can't simultate on Multiplier: " ++ show fb


-- |Для генерации процессоров и тестов использующих данный вычислительный блок используются
-- реализованные ниже функции. Вызов этих методов выполняется при генерации проекта с сетью,
-- включающей данный вычислительный блок либо при генерации его тестов.
instance ( Time t, Var v
         ) => TargetSystemComponent (Multiplier v x t) where
    -- |Наименование аппаратного модуля, экземпляр которого создаётся для его встраивания в
    -- процессор.
    moduleName _title _pu = "pu_mult"

    -- |Генератор программного обеспечения вычислительного блока. В случае умножителя ПО
    -- отсутствует. Разберёмся почему так. Ранее говорилось, что ПО имеет две составляющие:
    --
    -- 1. Настройки и начальные состояния, в случае с умножителем настройки специфичные для
    --    конкретного прикладного алгоритма отсутствуют.
    -- 2. Микропрограмма. В связи с тем, что вычислительный блок не может использоваться вне сетевой
    --    структуры процессора, определять ПО в контексте отдельного блока не является
    --    целесообразным. Кроме того, сигнальные линии отдельных вычислительных блоков могут быть
    --    мультиплексированы. В связи с этим, микропрограмма формируется сразу для сети
    --    вычислительных блоков, путём слияния их микрограмм, строемых на базе описаний
    --    вычислительного процесса (см. 'NITTA.BusNetwork').
    software _ _ = Empty

    -- |Генератор аппаратного обеспечения вычислительного блока. В случае с умножителем, генерация
    -- как токавая не производится. Умножитель описывается двумя файлами: (1) умножитель
    -- непосредственно, реализуемый либо IP ядром, либо функциональной заглушкой; (2) модуль
    -- реализующий интерфейс между непосредственно умножителем и инфраструктурой процессора.
    hardware title pu@Multiplier{ puMocked }
        = Aggregate Nothing
            [ if puMocked
                then FromLibrary "mult/mult_mock.v"
                else FromLibrary "mult/mult_inner.v"
            , FromLibrary $ "mult/" ++ moduleName title pu ++ ".v"
            ]

    -- |Генерация фрагмента исходного кода для создания экземпляра вычислительного блока в рамках
    -- процессора. Основная задача данной функции - корректно включить вычислительный блок в
    -- инфраструктуру процессора, установив все параметры, имена и провода.
    hardwareInstance title _pu Enviroment{ net=NetEnv{..}, signalClk, signalRst } PUPorts{..} = renderMST
        [ "pu_mult"
        , "  #( .DATA_WIDTH( " ++ show parameterDataWidth ++ " )"
        , "   , .ATTR_WIDTH( " ++ show parameterAttrWidth ++ " )"
        , "   , .INVALID( 0 )" -- FIXME:
        , "   ) $name$"
        , "  ( .clk( " ++ signalClk ++ " )"
        , "  , .rst( " ++ signalRst ++ " )"
        , "  , .signal_wr( " ++ signal wr ++ " )"
        , "  , .signal_sel( " ++ signal wrSel ++ " )"
        , "  , .data_in( " ++ dataIn ++ " )"
        , "  , .attr_in( " ++ attrIn ++ " )"
        , "  , .signal_oe( " ++ signal oe ++ " )"
        , "  , .data_out( " ++ dataOut ++ " )"
        , "  , .attr_out( " ++ attrOut ++ " )"
        , "  );"
        ] [("name", title)]
