{- |
Module      : NITTA.Model.Problems.Refactor
Description : Automatic manipulation over an intermediate representation
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Some times, CAD can not synthesis a target system because of a features of an
algorithm and microarchitecture (too less process units, too many functions, too
complicated algorithm).

In this case user can manually add some tweaks to the algorithm, but for that he
should be an expert with deep understanding of NITTA project. Of course, it is
not acceptable. This module defines type of that tweaks.
-}
module NITTA.Model.Problems.Refactor (
    module NITTA.Model.Problems.Refactor.BreakLoop,
    module NITTA.Model.Problems.Refactor.OptimizeAccum,
    module NITTA.Model.Problems.Refactor.OptimizeLut,
    module NITTA.Model.Problems.Refactor.ResolveDeadlock,
    module NITTA.Model.Problems.Refactor.ConstantFolding,
) where

import NITTA.Model.Problems.Refactor.BreakLoop
import NITTA.Model.Problems.Refactor.ConstantFolding
import NITTA.Model.Problems.Refactor.OptimizeAccum
import NITTA.Model.Problems.Refactor.OptimizeLut
import NITTA.Model.Problems.Refactor.ResolveDeadlock
