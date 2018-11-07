{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-|
Module : Multiplier (модель вычислительного блока умножителя)

В данном модуле реализуется модель вычислительного блока умножителя для САПР. Он может вычислять
следующие функции:

- 'NITTA.Functions.Multiply'.

В один момент времени может вычисляться только одна функция, при этом ее выполнение не может быть
прервано.

Данный модуль следует рассматривать как образец при реализации других моделей вычислительных блоков.
Его исходный код написан практически в «литературном стиле», в связи с чем рекомендуем продолжить
чтение из исходного кода.


= Пример работы

Рассмотрим пример планирования вычислительного процесса для одной функции. Для этого, запустим
интерпретатор ghci, для чего из папки проекта необходимо выполнить @stack repl@. Высока вероятность,
что с актуальной версией проекта вывод может незначительно отличаться.

Подключаем необходимые модули и настраиваем вид приглашения терминала.

>>> :l NITTA.ProcessUnits.Multiplier
[ 1 of 10] Compiling NITTA.Types.Poly ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Poly.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Poly.o ) [flags changed]
[ 2 of 10] Compiling NITTA.Types.Time ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Time.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Time.o ) [flags changed]
[ 3 of 10] Compiling NITTA.Types.Base ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Base.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Base.o ) [flags changed]
[ 4 of 10] Compiling NITTA.Types.Network ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Network.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Network.o ) [flags changed]
[ 5 of 10] Compiling NITTA.Types      ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types.o ) [flags changed]
[ 6 of 10] Compiling NITTA.Utils.Lens ( /Users/penskoi/Documents/src/nitta/src/NITTA/Utils/Lens.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Utils/Lens.o ) [flags changed]
[ 7 of 10] Compiling NITTA.Utils      ( /Users/penskoi/Documents/src/nitta/src/NITTA/Utils.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Utils.o ) [flags changed]
[ 8 of 10] Compiling NITTA.Functions ( /Users/penskoi/Documents/src/nitta/src/NITTA/Functions.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Functions.o ) [flags changed]
[10 of 10] Compiling NITTA.ProcessUnits.Multiplier ( /Users/penskoi/Documents/src/nitta/src/NITTA/ProcessUnits/Multiplier.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/ProcessUnits/Multiplier.o )
Ok, 10 modules loaded.
>>> :module +NITTA.Types NITTA.Functions Numeric.Interval Data.Set
>>> :set prompt "\ESC[34mλ> \ESC[m"

Создаём функцию и начальное состояние модели вычислительного блока умножителя. К сожалению, GHC не
хватает информации из контекста, чтобы вывести их типы, по этому зададим их явно.

>>> let f = multiply "a" "b" ["c", "d"] :: F (Parcel String Int)
>>> f
<Multiply (I "a") (I "b") (O (fromList ["c","d"]))>
>>> let st0 = multiplier True :: Multiplier String Int Int
>>> st0
Multiplier {puRemain = [], targets = [], sources = [], doneAt = Nothing, process_ = Process {steps = [], relations = [], nextTick = 0, nextUid = 0}, isMocked = True}
>>> options endpointDT st0
[]

Назначим функцию на выполнение вычислительному блоку. Данная операция может быть выполнена в любой
момент работы с моделью, в том числе когда вычислительный процесс уже полностью спланирован
(добавление новой работы). Главное правило - если работа назначена, то она обязана быть выполнена и
не может быть "потеряна" внутри модели. Если у вычислительного блока есть внутренние ресурсы - то их
должно быть достаточно для завершения планирования, даже если оно неэффективно.

>>> let Right st1 = tryBind f st0
>>> st1
Multiplier {puRemain = [<Multiply (I "a") (I "b") (O (fromList ["c","d"]))>], targets = [], sources = [], doneAt = Nothing, process_ = Process {steps = [], relations = [], nextTick = 0, nextUid = 0}, isMocked = True}
>>> mapM_ print $ options endpointDT st1
?Target "a"@(0..∞ /P 1..∞)
?Target "b"@(0..∞ /P 1..∞)

Как можно видеть, после назначения у нас появилось два варианта развития вычислительного процесса,
соответствующих разным последовательностям загрузки аргументов: сперва загрузить переменную @a@ или
@b@. При этом видно, что они идентичны с точки зрения времени исполнения: загрузка может быть начата
с 0 такта или спустя произвольную задержку; для загрузки аргумента необходим 1 такт, но она может
длиться произвольное время. Выберем один из вариантов (отметим, если решение соответствует
предложенным вариантам - то при его принятии не могут возникнуть ошибки или не могут быть полностью
заблокированы другие функции):

>>> let st2 = decision endpointDT st1 $ EndpointD (Target "a") (0...2)
>>> st2
Multiplier {puRemain = [], targets = ["b"], sources = ["c","d"], doneAt = Nothing, process_ = Process {steps = [Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 3, nextUid = 2}, isMocked = True}
>>> mapM_ print $ options endpointDT st2
?Target "b"@(3..∞ /P 1..∞)
>>> let st3 = decision endpointDT st2 $ EndpointD (Target "b") (3...3)
>>> st3
Multiplier {puRemain = [], targets = [], sources = ["c","d"], doneAt = Just 6, process_ = Process {steps = [Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 4, nextUid = 4}, isMocked = True}
>>> mapM_ print $ options endpointDT st3
?Source (fromList ["c","d"])@(6..∞ /P 1..∞)

После загрузки всех аргументов можно видеть, что следующим вариантом является выгрузка переменных
@c@ и @d@ из вычислительного блока умножителя. Важно отметить, что переменные могут выгружаться как
параллельно, так и последовательно (подробнее, см. принципы работы архитектуры процессора).
Рассмотрим второй вариант:

>>> let st4 = decision endpointDT st3 $ EndpointD (Source $ fromList ["c"]) (6...6)
>>> st4
Multiplier {puRemain = [], targets = [], sources = ["d"], doneAt = Just 6, process_ = Process {steps = [Step {sKey = 5, sTime = Activity (6 ... 6), sDesc = Out},Step {sKey = 4, sTime = Activity (6 ... 6), sDesc = Source (fromList ["c"])},Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 7, nextUid = 6}, isMocked = True}
>>> mapM_ print $ options endpointDT st4
?Source (fromList ["d"])@(7..∞ /P 1..∞)
>>> let st5 = decision endpointDT st4 $ EndpointD (Source $ fromList ["d"]) (7...7)
>>> st5
Multiplier {puRemain = [], targets = [], sources = [], doneAt = Nothing, process_ = Process {steps = [Step {sKey = 7, sTime = Activity (7 ... 7), sDesc = Out},Step {sKey = 6, sTime = Activity (7 ... 7), sDesc = Source (fromList ["d"])},Step {sKey = 5, sTime = Activity (6 ... 6), sDesc = Out},Step {sKey = 4, sTime = Activity (6 ... 6), sDesc = Source (fromList ["c"])},Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 8, nextUid = 8}, isMocked = True}
>>> options endpointDT st5
[]

Варианты развития вычислительного процесса закончились. Все назначеные функции выполнены. Далее
может быть сгенерирован микрокод, организующий описанный вычислительный процесс.
-}

-- FIXME: Перспективным направлением по развитию данного вычислительного блока является внедрение в него регистра
-- накопителя, что позволит перемножать произвольное количество аргументов, что сократит количество транзакций по шине
-- данных при перемножении более двух переменных одной функцией.

module NITTA.ProcessUnits.Multiplier
    ( multiplier
    , Multiplier
    , PUPorts(..)
    ) where

import           Control.Monad                 (when)
import           Data.Default
import           Data.List                     (find, partition, (\\))
import           Data.Set                      (elems, fromList, member)
import           Data.Typeable
import qualified NITTA.Functions               as F
import           NITTA.Project
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Process
import           Numeric.Interval              (inf, sup, (...))
import           Text.InterpolatedString.Perl6 (qc)

{-
= Вычислительный блок

Вычислительные блоки могут реализовывать:

- хранение и обработку данных;
- взаимодействие с периферией;
- управление и контроль за вычислителем.

При этом, они характеризуются сложным поведением, выраженным в:

- многофункциональности;
- внутреннем параллелизме;
- суперскалярности;
- конвейеризации;
- наличие внутренних ресурсов.

Рассматриваемый вычислительный блок с этих точек зрения является одним из простейших, Так как
реализует только обработку данных согласно одной функции.

Поведение вычислительного блока определяется прикладным алгоритмом, являющимся композицией функций с
зависимостями по данным ('NITTA.Functions'). САПР назначает (привязывает) функции к конкретным
вычислительным блокам, а те, в свою очередь, определяют возможные варианты развития процесса.

Любой вычислительный блок подразумевает три составляющие:

- аппаратное обеспечение вычислительного блока - набор заранее подготовленных либо автоматически
  генерируемых файлов описания аппаратруры на Hardware Description Language (@/hdl/multiplier@);
- программное обеспечение вычислительного блока - набор бинарных файлов задающих:
  - начальное состояние и настройки вычислительного блока;
  - управляющую программу;
- модель вычислительного блока в САПР - компонент САПР, реализующий поддержку вычислительного блока
  (генерация аппаратной и программной составляющей, объединение вычислительных блоков в процессора,
  планирование вычислительного процесса и т.д.).

При этом все три составляющие являются сильно связанными между собой и должны строго друг-другу
соответствовать. Для глубокого понимания принципов функционирования вычислительного блока необходимо
иметь представление обо всех его частях. Ниже будет подробно рассмотрена модель вычислительного
блока умножителя и то, как она реализована.
-}

{-
* Модель вычислительного блока

Целью модели вычислительного блока является «научить» САПР работать с вычислительным блоком, а
именно:

- какие функции могут быть вычислены с его помощью (см. 'NITTA.Type.ProcessUnit');
- назначить экземпляру вычислительного блока выполнение указанной функции (см.
  'NITTA.Type.ProcessUnit');
- система команд вычислительного блока и структура микрокода, позволяющая им управлять (см.
  'NITTA.Type.Controllable');
- преобразовать инструкции в микрокод (см. 'NITTA.Type.UnambiguouslyDecode');
- какие есть варианты (@options@) развития вычислительного процесса (загрузить или выгрузить ту или
  иную переменную или группу переменных);
- спланировать вычислительный процесс, описываемый загрузкой или выгрузкой переменных в или из
  вычислительного блока (см. @decision@).
-}

{-|
Основой модели вычислительного блока является структура данных, фиксирующая:

- состояние вычислительного блока на протяжении всего планирования вычислительного процесса;
- описание вычислительного процесса (целиком или фрагмента), которое может быть транслировано в
  программное обеспечение.

Именно вокруг данной структуры данных и строится вся алгоритмическая часть модели вычислительного
блока. Структура данных параметризуется следующими переменными типа:

- v - идентификатор перемменой;
- x - тип значений, с которыми работает умножитель;
- t - идентификатор момента времени.
-}

-- FIXME: Разработать safeDecision, которая будет проверять осуществлять дополнительные проверки
-- корректности принятого решения.

-- FIXME: Убрать сигнал wrSignal.
data Multiplier v x t
    = Multiplier
        { -- |Список назначенных, но еще необработанных или необрабатываемых функций.
          -- Выполнение функции начинается с:
          --
          -- - удаления функции из данного списка;
          -- - переноса информации из функции в поля 'targets' и 'sources' ('assignment'), далее
          --   такая функция будет именоваться текущей.
          --
          -- Назначенные функции могут выполняться в произвольном порядке. Явное хранение информации
          -- о выполненных функциях не осуществляется, так как она есть в описание вычислительного
          -- процесса 'process_'.
          remain               :: [F (Parcel v x)]
          -- |Список переменных, которые необходимо загрузить в вычислительный блок для вычисления
          -- текущей функции.
        , targets              :: [v]
          -- |Список переменных, которые необходимо выгрузить из вычислительного блока для
          -- вычисления текущей функции. Порядок выгрузки - произвольный. Важно отметить, что все
          -- выгружаемые переменные соответствуют одному значению - результату умножения.
        , sources              :: [v]
          -- |Фактический процесс умножения будет завершён в указанный момент времени и его
          -- результат будет доступен для выгрузки. Значение устанавливается сразу после загрузки
          -- всех аргументов.
        , doneAt               :: Maybe t
        , currentWork          :: Maybe (t, F (Parcel v x))
          -- |В процессе планирования вычисления функции необходимо определить неопределённое
          -- количество загрузок / выгрузок данных в / из вычислительного блока, что бы затем
          -- установить вертикальную взаимосвязь между информацией о выполняемой функции и этими
          -- пересылками.
        , currentWorkEndpoints :: [ ProcessUid ]
          -- |Описание вычислительного процесса, спланированного для данного вычислительного блока
          -- 'NITTA.Types.Base.Process'.
        , process_             :: Process v x t
        , tick                 :: t
          -- |В реализации данного вычислительного блока используется IP ядро поставляемое вместе с
          -- Altera Quartus. Это не позволяет осуществлять симуляцию при помощи Icarus Verilog.
          -- Чтобы обойти данное ограничение была создана заглушка, подключаемая вместо IP ядра если
          -- установлен данный флаг.
        , isMocked             :: Bool
        }
    deriving ( Show )


-- |Отслеживание внутренних зависимостей по данным, формируемым вычислительным блоком.
instance Locks (Multiplier v x t) v where
    locks Multiplier{ remain, sources, targets } =
        -- Зависимость выходных данных от загружаемых аргументов. Если @sources@ пустой список,
        -- то и зависимостей не будет.
        [ Lock{ lockBy, locked }
        | locked <- sources
        , lockBy <- targets
        ]
        ++
        -- Зависимости функций в очереди от выполняемой в настоящий момент.
        [ Lock{ lockBy, locked }
        | locked <- concatMap (elems . variables) remain
        , lockBy <- sources ++ targets
        ]


-- |Конструктор модели умножителя вычислительного блока. Аргумент определяет внутреннюю оранизацию
-- вычислительного блока: использование IP ядра умножителя (False) или заглушки (True). Подробнее
-- см. функцию hardware в классе 'TargetSystemComponent'.
multiplier mock = Multiplier
    { remain=[]
    , targets=[]
    , sources=[]
    , doneAt=Nothing
    , currentWork=Nothing
    , currentWorkEndpoints=[]
    , process_=def
    , tick=def
    , isMocked=mock
    }


-- |Привязку функций к вычислительным блокам осуществляет данный класс типов. Он позволяет
-- проверить, может ли функция быть вычислена данным вычислительным блоком и если да - осуществляет
-- ее назначение. При этом отказ в привязке может быть связан как с тем, что данный тип функций не
-- поддерживается, так и с тем что исчерпаны внутрении ресурсы вычислительного блока.
--
-- С точки зрения САПР привязка выглядит следующим образом: САПР опрашивает модели всех
-- наличиствующих экземпляров вычислительных блоков и получает список тех, которые готовы взять в
-- работу рассматриваемую функцию. Затем, на основании различных метрик (как например загрузка
-- вычислительных блоков, количество и тип ещё не привязанных функций) выбирается лучший вариант.
-- Привязка может быть выполнена как постепенно по мере планирования вычислительного процесса, так и
-- одновременно для всех функций в самом начале.
instance ( Var v, Time t
         ) => ProcessUnit (Multiplier v x t) v x t where
    -- |Привязка к вычислительному блоку осуществялется этой функцией.
    tryBind f pu@Multiplier{ remain }
        -- Для этого осуществляется проверка, приводится ли тип функции к одному из поддерживаемых
        -- ('NITTA.FunctionalBlocks.castF') и в случае успеха возвращается состояние модели после
        -- привязки с меткой 'Right'.
        --
        -- Важно отметить, что "привязка" не означается фактическое начало работы, что позволяет
        -- сперва осуществить привязку всех задач, а уже потом планировать вычислительный процесс.
        | Just F.Multiply{} <- F.castF f = Right pu{ remain=f : remain }
        -- В случае невозможности привязки возвращается строка с кратким описание причины отказа и
        -- меткой 'Left'.
        | otherwise = Left $ "The function is unsupported by Multiplier: " ++ show f
    -- |Унифицированный интерфейс для получения описания вычислительного процесса.
    process = process_
    -- |Данный метод используется для установки времени вычислительного блока снаружи. В настоящий
    -- момент это необходимо только для реализации ветвления, которое находится на стадии
    -- прототипирования.
    setTime t pu@Multiplier{} = pu{ tick=t }


-- |Данная функция осуществляет фактическое взятие функционального блока в работу.
assignment pu@Multiplier{ targets=[], sources=[], remain, tick } f
    | Just (F.Multiply (I a) (I b) (O c)) <- F.castF f
    = pu
        { targets=[a, b]
        , currentWork=Just (tick + 1, f)
        , sources=elems c, remain=remain \\ [ f ]
        }
assignment _ _ = error "Multiplier: internal assignment error."



{-
Результатом планирования является описание одного вычислительного цикла, которое в последствии может
быть транслировано в микрокод, непосредственно управляющий вычислительным блоком. С точки зрения
архитектуры NITTA, процесс может быть описан как последовательное выполнение вычислительным блоком
двух ролей:

- источника данных ('Source');
- получателя данных ('Target').

Сам же процесс планирования состоит из двух операций выполняемых в цикле:
-}
instance ( Var v, Time t, Typeable x
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Multiplier v x t)
        where

    -- 1. Опрос вычислительного блока относительно того, в каких ролях он готов выступить (другими
    --    словами, как может развиваться вычислительный процесс). Он реализуется фунцией @options@,
    --    результатом которой является один из следующих списков:

    --    - список вариантов загружаемых в вычислительный блок переменных, необходимых для
    --      находящейся в работе функции;
    options _proxy Multiplier{ targets=vs@(_:_), tick }
        = map (\v -> EndpointO (Target v) $ TimeConstrain (tick + 1 ... maxBound) (1 ... maxBound)) vs

    --    - список вариантов выгружаемых из вычислительного блока переменных;
    options _proxy Multiplier{ sources, doneAt=Just at, tick }
        | not $ null sources
        = [ EndpointO (Source $ fromList sources) $ TimeConstrain (max at (tick + 1) ... maxBound) (1 ... maxBound) ]

    --    - список вариантов загружаемых в вычислительный блок переменных, загрузка любой из которых
    --      приведёт к фактическу началу работы над соответствующей функцией.
    options proxy pu@Multiplier{ remain } = concatMap (options proxy . assignment pu) remain

    --    Отметим, что предоставляемые данной функцией варианты требуют уточнений, так как:

    --    1. Указывают не конкретный момент времени для работы, а на доступный интервал
    --       ('NITTA.Types.Base.TimeConstrain'), описывающий с и по какой момент может
    --       осуществляться загрузка/выгрузка, а также сколько этот процесс может длиться.
    --    2. Одно значение может выгружаться из вычислительного блока как несколько различных
    --       переменных. Это может осуществляться как единомоментно (на уровне аппаратуры на шину
    --       выставляется значение и считывается сразу несколькими вычислительными блоками), так и
    --       последовательно (сперва значение на шину будет выставлено для одного вычислительного
    --       блока, а затем для другого), что также должно быть уточнено.

    -- 2. Планирование процесса или применение решения о развитии вычислительного процесса к
    --    состоянию модели вычислительного блока осуществляется функцией @decision@. Преобразование
    --    варианта полученного из @options@ осуществляется САПР за пределами модели вычислительного
    --    блока. Можно выделить следующие варианты решений:
    --
    --    1. Если модель ожидает, что в неё загрузят переменную, тогда:
    decision _proxy pu@Multiplier{ targets=vs, currentWorkEndpoints } d@EndpointD{ epdRole=Target v, epdAt }
        -- Из списка загружаемых значений извлекается трубуемая переменная, а остаток - сохраняется
        -- для следующих шагов.
        | ([_], xs) <- partition (== v) vs
        -- Переменная @sel@ используется для того, что бы зафиксировать очерёдность загрузки
        -- переменных в аппаратный блок, что необходимо из-за особенностей реализации.
        , let sel = if null xs then B else A
        -- Осуществляется планирование вычислительного процесса.
        , let (newEndpoints, process_') = runSchedule pu $ do
                -- костыль, необходимый для корректной работы автоматически сгенерированных тестов,
                -- которые берут информацию о времени из Process
                updateTick (sup epdAt)
                scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) $ Load sel
        = pu
            { process_=process_'
              -- Сохраняется остаток работы на следующий цикл.
            , targets=xs
              -- Сохраняем информацию тех событиях процесса, которые описываются отправку /
              -- получение данных для текущего функционального блока.
            , currentWorkEndpoints=newEndpoints ++ currentWorkEndpoints
              -- Если загружены все необходимые аргументы (@null xs@), то сохраняется момент
              -- времени, когда будет получен результат.
            , doneAt=if null xs
                then Just $ sup epdAt + 3
                else Nothing
              -- Продвигается вперёд модельное время.
            , tick=sup epdAt
            }
    --    2. Если модель ожидает, что из неё выгрузят переменные.
    decision _proxy pu@Multiplier{ targets=[], sources, doneAt, currentWork=Just (a, f), currentWorkEndpoints } d@EndpointD{ epdRole=Source v, epdAt }
        | not $ null sources
        , let sources' = sources \\ elems v
        , sources' /= sources
        -- Осуществляется планирование вычислительного процесса.
        , let (newEndpoints, process_') = runSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) Out
                when (null sources') $ do
                    high <- scheduleFunction a (sup epdAt) f
                    let low = endpoints ++ currentWorkEndpoints
                    -- Устанавливаем вертикальную взаимосвязь между функциональным блоком и
                    -- связанными с ним пересылками данных.
                    establishVerticalRelations high low
                -- костыль, необходимый для корректной работы автоматически сгенерированных тестов,
                -- которые берут информацию о времени из Process
                updateTick (sup epdAt)
                return endpoints
        = pu
            { process_=process_'
              -- В случае если не все переменные были запрошены - сохраняются оставшиеся.
            , sources=sources'
              -- Если вся работа выполнена, то сбрасывается время готовности результата, текущая
              -- работа и перечисление передач, выполненных в рамках текущей функции.
            , doneAt=if null sources' then Nothing else doneAt
            , currentWork=if null sources' then Nothing else Just (a, f)
            , currentWorkEndpoints=if null sources' then [] else newEndpoints ++ currentWorkEndpoints
              -- Продвигается вперёд модельное время.
            , tick=sup epdAt
            }
    --    3. Если никакая функция в настоящий момент не выполняется, значит необходимо найти в
    --       списке назначенных функций требуемую, запустить ее в работу, и только затем принять
    --       решение и спланировать фрагмент вычислительного процесса при помощи рекурсивного вызова
    --       в ситуации 1.
    decision proxy pu@Multiplier{ targets=[], sources=[], remain } d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `member` variables f) remain
        = decision proxy (assignment pu f) d
    --    4. Если что-то пошло не так.
    decision _ pu d = error $ "Multiplier decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d



-- |Идентификатор аргумента операции умножения.
--
-- Как ранее говорилось, из-за особенностей аппаратной реализации, в спланированом процессе на
-- уровне инструкций необходимо учитывать последовательность загрузки операндов. Для этого и
-- определён данный тип. При этом необходимо отметить, что с точки зрения алгоритма и модели порядок
-- аргументов не имеет значения, что отражено в реализованном выше классе, отвечающем за
-- планирование вычислительного процесса.
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
    -- используются при планировании вычислительного процесса функцией 'simpleSynthesis'. Кроме них, неявно
    -- присутствует инструкция @nop@ - когда никаких действий не выполняется.
    data Instruction (Multiplier v x t)
        = Load ArgumentSelector
        | Out
        deriving (Show)

    -- |Набор сигналов для управления вычислительным блоком и представления микрокода для данного
    -- вычислительного блока.
    data Microcode (Multiplier v x t)
        = Microcode
            { -- |Сигнал записи в вычислительный блок.
              wrSignal :: Bool
              -- |Селектор аргумента, загружаемого в вычислительный блок.
            , selSignal :: Bool
              -- |Сигнал выгрузки результата из вычислительного блока.
            , oeSignal :: Bool
            }
        deriving ( Show, Eq, Ord )

-- |Также, для микрокода необходимо определить состояние по умолчанию (соответствует неявной
-- инструкции @nop@), которое означает, что вычислительный блок находится в состоянии бездействия,
-- при этом не занимает шину и хранит своё внутренее состояние в предсказуемом виде. В случае
-- умножителя - не сбрасывает результат умножения и не работает с шиной. Состояние по умолчанию
-- используется для остановки, паузы или ожидания вычислительного блока.
instance Default (Microcode (Multiplier v x t)) where
    def = Microcode
        { wrSignal=False
        , selSignal=False
        , oeSignal=False
        }

-- |Связка инструкций и микрокода осуществляется данным классом, который требует их однозначного
-- соответствия, а также независимо от состояния и настроек модели.

-- TODO: Требуется привязка к PU, так как в настройках модели может определяться ширина тех или иных
-- значений (как в случае с addr у Fram)
instance UnambiguouslyDecode (Multiplier v x t) where
    decodeInstruction (Load A) = def{ wrSignal=True, selSignal=False }
    decodeInstruction (Load B) = def{ wrSignal=True, selSignal=True }
    decodeInstruction Out      = def{ oeSignal=True }

-- |Определение сигнальных линий вычислительного блока, используемого для их ручного подключения к
-- сигнальной шине на уровне сети, а также функция отображения микрокода на линии. В будущем данный
-- класс будет перерабатываться с целью автоматизации данного процесса.
instance Connected (Multiplier v x t) where
    data PUPorts (Multiplier v x t)
        = PUPorts
            { wr           -- ˆЗагрузить аргумент.
            , wrSel        -- ˆВыбор загружаемого аргумента (A | B).
            , oe :: Signal -- ˆВыгрузить результат работы.
            } deriving ( Show )
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
    simulateOn cntx _ f
        -- Определяем функцию и делегируем ее расчет реализации по умолчанию.
        | Just f'@F.Multiply{} <- F.castF f = simulate cntx f'
        | otherwise = error $ "Can't simultate on Multiplier: " ++ show f


-- |Для генерации процессоров и тестов использующих данный вычислительный блок используются
-- реализованные ниже функции. Вызов этих методов выполняется при генерации проекта с сетью,
-- включающей данный вычислительный блок или при генерации тестов.
instance ( Time t, Var v
         ) => TargetSystemComponent (Multiplier v x t) where
    -- |Наименование аппаратного модуля, экземпляр которого создаётся для его встраивания в
    -- процессор. В данном случае задается в файле @/hdl/multiplier/pu_multiplier.v@.
    moduleName _title _pu = "pu_multiplier"

    -- |Генератор программного обеспечения вычислительного блока. В случае умножителя ПО
    -- отсутствует. Разберёмся почему так. Ранее говорилось, что ПО имеет две составляющие:
    --
    -- 1. Настройки и начальные состояния, в случае с умножителем настройки специфичные для
    --    конкретного прикладного алгоритма отсутствуют.
    -- 2. Микропрограмма. В связи с тем, что вычислительный блок не может использоваться вне сетевой
    --    структуры процессора, определять ПО в контексте отдельного блока не является
    --    целесообразным. Кроме того, сигнальные линии отдельных вычислительных блоков могут быть
    --    мультиплексированы. В связи с этим, микропрограмма формируется сразу для сети
    --    вычислительных блоков путём слияния их микрограмм, генерируемых на базе описаний
    --    вычислительного процесса (см. 'NITTA.BusNetwork').
    software _ _ = Empty

    -- |Генератор аппаратного обеспечения вычислительного блока. В случае с умножителем, генерация
    -- как таковая не производится. Умножитель описывается двумя файлами: (1) умножитель
    -- непосредственно, реализуемый либо IP ядром, либо функциональной заглушкой; (2) модуль
    -- реализующий интерфейс между непосредственно умножителем и инфраструктурой процессора.
    hardware title pu@Multiplier{ isMocked }
        = Aggregate Nothing
            [ if isMocked
                then FromLibrary "multiplier/mult_mock.v"
                else FromLibrary "multiplier/mult_inner.v"
            , FromLibrary $ "multiplier/" ++ moduleName title pu ++ ".v"
            ]

    -- |Генерация фрагмента исходного кода для создания экземпляра вычислительного блока в рамках
    -- процессора. Основная задача данной функции - корректно включить вычислительный блок в
    -- инфраструктуру процессора, установив все параметры, имена и провода.
    hardwareInstance title _pu Enviroment{ net=NetEnv{..}, signalClk, signalRst } PUPorts{..}
        = [qc|pu_multiplier #
        ( .DATA_WIDTH( {parameterDataWidth} )
        , .ATTR_WIDTH( {parameterAttrWidth} )
        , .INVALID( 0 )  // FIXME: Сделать и протестировать работу с атрибутами.
        ) { title }
    ( .clk( {signalClk} )
    , .rst( {signalRst} )
    , .signal_wr( { signal wr } )
    , .signal_sel( { signal wrSel } )
    , .data_in( { dataIn } )
    , .attr_in( { attrIn } )
    , .signal_oe( { signal oe } )
    , .data_out( { dataOut } )
    , .attr_out( { attrOut } )
    );|]


-- |Данный класс является служебным и предназначен для того, что бы извлекать из вычислительного
-- блока все функции, привязанные к вычислительному блоку. Реализация данного класс проста: у модели
-- вычистельного блока берётся описание процесса (все спланированные функции), берутся все функции в
-- очереди и, в случае наличия, функция находящаяся в работе.
instance ( Ord t ) => WithFunctions (Multiplier v x t) (F (Parcel v x)) where
    functions Multiplier{ process_, remain, currentWork }
        = functions process_
        ++ remain
        ++ case currentWork of
            Just (_, f) -> [f]
            Nothing     -> []


-- |Основное назначение данного класс - генерация автоматизированных тестов изолировано для данного
-- вычислительного блока. Для этого он позволяет сгенерировать test bench для вычисилтельного блока
-- в соответствии с его моделью и спланированным вычислительным процессом. С самими тестами следует
-- ознакомиться в 'Spec'.
--
-- Тестирование осуществляется следующим образом: на основании имеющегося описания вычислительного
-- процесса генерируется последовательность внешних воздействия на вычислительный блок (сигналы и
-- входные данные), а также последовательность проверок выходных сигналов и данных. Выходные данные
-- сравниваются с результатами функциональной симуляции и если они не совпадают, то выводится
-- соответствующее сообщение об ошибке.
instance ( Var v, Time t
         , Typeable x, Show x, Integral x
         ) => TestBench (Multiplier v x t) v x where
    testBenchDescription prj@Project{ projectName, processorModel }
        -- Test bench представляет из себя один файл описанный ниже. Для его генерации используется
        -- готовый snippet, так как в большинстве случаев они будут подобны. Ключевое значение имеет
        -- структура данных 'NITTA.Project.TestBenchSetup', описывающая специфику данного модуля.
        = Immidiate (moduleName projectName processorModel ++ "_tb.v")
            $ snippetTestBench prj TestBenchSetup
                -- Список управляющих сигналов. Необходим для инициализации одноименных регистров.
                { tbcSignals=["oe", "wr", "wrSel"]
                -- Функция подключения вычислительного блока к окружению и идентификаторы сигнальных
                -- линий. В структуре @tbcPorts@ описывается к чему именно подключаются сигнальные
                -- линии тестируемого блока. А в @tbcSignalConnect@ как эти абстрактные номера
                -- отображаются в генерируемом исходном коде.
                , tbcPorts=PUPorts
                    { oe=Signal 0
                    , wr=Signal 1
                    , wrSel=Signal 2
                    }
                , tbcSignalConnect= \case
                    (Signal 0) -> "oe"
                    (Signal 1) -> "wr"
                    (Signal 2) -> "wrSel"
                    _ -> error "testBenchDescription wrong signal"
                -- При генерации test bench-а знать, как задаются управляющие сигналы
                -- вычислительного блока. Именно это и описано ниже. Отметим, что работа с шиной данных полностью реализуется в рамках snippet-а.
                , tbcCtrl= \Microcode{ oeSignal, wrSignal, selSignal } ->
                    [qc|oe <= {bool2verilog oeSignal}; wr <= {bool2verilog wrSignal}; wrSel <= {bool2verilog selSignal};|]
                }
