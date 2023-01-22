{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}


module Echidna.Types.Logger where


import Echidna.Types.Coverage (PC, OpIx)
import Echidna.Types.Tx (TxCall)
import Echidna.Pretty (ppTxCall)


type StepInfo = (PC, OpIx)

type TxInfo = (TxCall, [StepInfo])

type SequenceInfo = [TxInfo]

type Logger = [SequenceInfo]

fppLogger :: Logger -> String
fppLogger x = "{\n" ++ ppLogger x ++ "\n }"

ppLogger :: Logger -> String
ppLogger (x:xs) = fppSequence x ++ ppLogger xs
ppLogger [] = ""

fppSequence :: SequenceInfo -> String
fppSequence x = "  seq[\n" ++ ppSequence x ++ "\n ] \n"

ppSequence :: [TxInfo] -> String
ppSequence (x:xs) = fppTxInfo x ++ ppSequence xs
ppSequence [] = ""

fppTxInfo :: TxInfo -> String
fppTxInfo x = "     tx[" ++ ppTxInfo x ++ "]\n"

ppTxInfo :: (TxCall, [StepInfo]) -> String
ppTxInfo (txCall, path) = ppTxCall txCall ++ ", " ++ ppPath path


fppPath :: [StepInfo] -> String
fppPath x = "path[" ++ ppPath x ++ "]"

ppPath :: [StepInfo] -> String
ppPath (x:xs) = fppStepInfo x ++ ", " ++ ppPath xs
ppPath [] = ""

fppStepInfo :: StepInfo -> String
fppStepInfo (x, y) = "(PC: " ++ show x ++ ", OpIx: " ++ show y ++ ")"