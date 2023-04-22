{-# LANGUAGE FlexibleContexts #-}

module Echidna.Mutator.Corpus where

import Control.Monad.Random.Strict (MonadRandom, getRandomR, weighted)
import Control.Monad.State.Strict (MonadState(..))
import Data.Has (Has(..))

import qualified Data.Set as DS

import Echidna.Types.Tx (Tx)
import Echidna.Types.Corpus
import Echidna.Transaction (mutateTx, shrinkTx)
import Echidna.ABI (GenDict)
import Echidna.Mutator.Array
import Echidna.Types.Coverage (CoverageSequenceFrequences)
import Data.Map (findWithDefault)

defaultMutationConsts :: Num a => MutationConsts a
defaultMutationConsts = (1, 1, 1, 1)

fromConsts :: Num a => MutationConsts Integer -> MutationConsts a
fromConsts (a, b, c, d) = let fi = fromInteger in (fi a, fi b, fi c, fi d)

data TxsMutation = Identity
                 | Shrinking
                 | Mutation
                 | Expansion
                 | Swapping
                 | Deletion
  deriving (Eq, Ord, Show)

data CorpusMutation = RandomAppend TxsMutation
                    | RandomPrepend TxsMutation
                    | RandomSplice
                    | RandomInterleave
  deriving (Eq, Ord, Show)

mutator :: MonadRandom m => TxsMutation -> [Tx] -> m [Tx]
mutator Identity  = return
mutator Shrinking = mapM shrinkTx
mutator Mutation = mapM mutateTx
mutator Expansion = expandRandList
mutator Swapping = swapRandList
mutator Deletion = deleteRandList

selectAndMutate :: MonadRandom m
                => ([Tx] -> m [Tx]) -> Corpus -> CoverageSequenceFrequences -> m [Tx]
selectAndMutate f ctxs freqMap = do
  rtxs <- weighted $ computeEnergies ctxs freqMap -- utiliza el orden de inserción como peso de probabilidad de elegir un elem del corpus (mas nuevo -> mas probable)
  k <- getRandomR (0, length rtxs - 1)
  f $ take k rtxs -- elige una cantidad random de primeras transacciones de la secuencia del corpus elegida

-- ctxs -> corpus transactions, gtxs -> generated transactions
-- combina dos sec del corpus usando la funcion dada y si faltan elems para llegar a ql, añade de las transacciones generadas.
selectAndCombine :: MonadRandom m
                 => ([Tx] -> [Tx] -> m [Tx]) -> Int -> Corpus -> [Tx] -> CoverageSequenceFrequences -> m [Tx]
selectAndCombine f ql ctxs gtxs freqMap = do
  let computedEnergies = computeEnergies ctxs freqMap
  let selectFromCorpus = weighted computedEnergies
  rtxs1 <- selectFromCorpus
  rtxs2 <- selectFromCorpus
  txs <- f rtxs1 rtxs2
  return . take ql $ txs ++ gtxs

computeEnergies :: Corpus -> CoverageSequenceFrequences -> [([Tx], Rational)]
computeEnergies corpus coverageSeqFreq =
  map (\(sequenceCoverage, txs) -> (txs, 1 / fromIntegral (findWithDefault 0 sequenceCoverage coverageSeqFreq))) $ DS.toList corpus

-- ctxs -> corpus transactions, gtxs -> generated transactions
getCorpusMutation :: (MonadRandom m, Has GenDict x, MonadState x m)
                  => CorpusMutation -> (Int -> Corpus -> [Tx] -> CoverageSequenceFrequences -> m [Tx])
getCorpusMutation (RandomAppend m) = mut (mutator m) -- mutator te da la funcion que aplica la mutacion elegida
 where mut f ql ctxs gtxs freqMap = do
          rtxs' <- selectAndMutate f ctxs freqMap
          return . take ql $ rtxs' ++ gtxs -- elige una cantidad mutada de transacciones en base a un elem del corpus y lo añade al ppio de las txs generadas, se eligen ql elems
getCorpusMutation (RandomPrepend m) = mut (mutator m)
 where mut f ql ctxs gtxs freqMap = do
          rtxs' <- selectAndMutate f ctxs freqMap
          k <- getRandomR (0, ql - 1)
          return . take ql $ take k gtxs ++ rtxs' -- elige una cant random de txs generadas a las que agrega al final una cantidad mutada de transacciones en base a un elem del corpus
getCorpusMutation RandomSplice = selectAndCombine spliceAtRandom -- elige dos secuencias del corpus, las corta en lugares random y pega la primer parte de uno y la segunda del otro, completa con gtxs
getCorpusMutation RandomInterleave = selectAndCombine interleaveAtRandom -- elige dos secs del corpus, toma las primeras partes en lugares random y las intercala, completa con gtxs

seqMutatorsStateful :: MonadRandom m => MutationConsts Rational -> m CorpusMutation
seqMutatorsStateful (c1, c2, c3, c4) = weighted
  [(RandomAppend Identity,   800),
   (RandomPrepend Identity,  200),

   (RandomAppend Shrinking,  c1),
   (RandomAppend Mutation,   c2),
   (RandomAppend Expansion,  c3),
   (RandomAppend Swapping,   c3),
   (RandomAppend Deletion,   c3),

   (RandomPrepend Shrinking, c1),
   (RandomPrepend Mutation,  c2),
   (RandomPrepend Expansion, c3),
   (RandomPrepend Swapping,  c3),
   (RandomPrepend Deletion,  c3),

   (RandomSplice,            c4),
   (RandomInterleave,        c4)
 ]

seqMutatorsStateless :: MonadRandom m => MutationConsts Rational -> m CorpusMutation
seqMutatorsStateless (c1, c2, _, _) = weighted
  [(RandomAppend Identity,   800),
   (RandomPrepend Identity,  200),

   (RandomAppend Shrinking,  c1),
   (RandomAppend Mutation,   c2),

   (RandomPrepend Shrinking, c1),
   (RandomPrepend Mutation,  c2)
  ]
