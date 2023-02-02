module Echidna.Types.Coverage where

import Control.Lens
import Data.Set (Set, size, map, foldr)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map, foldrWithKey)

import Echidna.Types.Tx (TxResult)
import qualified Data.HashMap.Internal.Strict as Map

-- Program Counter directly obtained from the EVM
type PC = Int
-- Index per operation in the source code, obtained from the source mapping 
type OpIx = Int
-- Stack size from the EVM
type FrameCount = Int
-- Basic coverage information
type CoverageInfo = (PC, OpIx, FrameCount, TxResult) -- el TxResult es el correspondiente al resultado de la ejecucion que genero ese CoverageInfo
-- Map with the coverage information needed for fuzzing and source code printing 
type CoverageMap = Map ByteString (Set CoverageInfo) -- key: contract bytecode, value: set of all positions in the contract executed at a certain time
-- Map with the frequence (amount of times it was covered) of a given sequence 
type CoverageSequenceFrequences = Map SequenceCoverage Int
-- A sequence coverage is determined by the set of PC executed for a given contract
type SequenceCoverage = Map ByteString (Set PC)
-- Energy used to define the probability of choosing a sequence or transaction
type Energy = Int

-- | Given good point coverage, count unique points.
coveragePoints :: CoverageMap -> Int
coveragePoints = sum . fmap size
-- | Given good point coverage, count the number of unique points but
-- only considering the different instruction PCs (discarding the TxResult).
-- This is useful to report a coverage measure to the user
scoveragePoints :: CoverageMap -> Int
scoveragePoints = sum . fmap (size . Data.Set.map (view _1))

ppCoverageSequenceFrequences :: Map SequenceCoverage Int -> String
ppCoverageSequenceFrequences = foldrWithKey (\k v acc -> ppSequence k ++ "\nvalue : " ++ show v ++ "\n" ++ acc) ""

ppSequence :: SequenceCoverage -> String
ppSequence x = "{\n" ++ rppSequence x ++ "\n}\n"

rppSequence :: Map ByteString (Set PC) -> String
rppSequence = foldrWithKey (\contract v acc -> "contract: " ++ show contract ++ "\nvalue : " ++ ppSetPC v ++ "\n" ++ acc) ""

ppSetPC :: Set PC -> String
ppSetPC x = "setPC(" ++ rppSetPC x ++ ")\n"

rppSetPC :: Set PC -> String
rppSetPC = Data.Set.foldr (\pc accum -> ppPC pc ++ accum) ""

ppPC :: PC -> String
ppPC x = show x ++ ", "

