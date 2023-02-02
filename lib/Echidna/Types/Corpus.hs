module Echidna.Types.Corpus where

import Data.Set (Set, size)
import Echidna.Types.Tx (Tx)
import Echidna.Types.Coverage (SequenceCoverage)

type InitialCorpus = (Int, [[Tx]])
type Corpus = Set (SequenceCoverage, [Tx])

corpusSize :: Corpus -> Int
corpusSize = size

type MutationConsts a = (a, a, a, a)
