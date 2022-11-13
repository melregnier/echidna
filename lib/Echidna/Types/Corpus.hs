module Echidna.Types.Corpus where

import Data.Set (Set, size)
import Echidna.Types.Tx (Tx)

type InitialCorpus = (Int, [[Tx]])
type Corpus = Set (Integer, [Tx]) -- entendemos que Integer es el orden en que fue añadido la se al corpus

corpusSize :: Corpus -> Int
corpusSize = size
