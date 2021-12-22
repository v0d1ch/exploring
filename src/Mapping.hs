{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Mapping where

import           RIO
import qualified RIO.Map as M

data AllQuestions = AllQuestions
    { callIdAQU   :: Text
    , questionAQU :: Text
    , scoreAQU    :: Int
    }
    deriving (Show, Eq)

data Questions = Questions
    { callIdQU   :: Text
    , questionQU :: Text
    , scoreQU    :: Int
    }
    deriving (Show, Eq)

data CallReportCSV = CallReportCSV
    { callId           :: Text
    , agentDisplayName :: Text
    , questions        :: Map Text Int
    }
    deriving (Show, Eq)

allQuestions' :: [AllQuestions]
allQuestions' = [ AllQuestions "id1" "" 0
                , AllQuestions "id2" "" 0
                , AllQuestions "id3" "" 0
                ]

callReports' :: [CallReportCSV]
callReports' = [ CallReportCSV "id1" "name1" mempty
               , CallReportCSV "id2" "name2" mempty
               , CallReportCSV "id3" "name3" mempty
               ]

questions' :: [Questions]
questions' = [ Questions "id1" "Q1" 10
             , Questions "id1" "Q2" 20
             , Questions "id2" "Q3" 30
             , Questions "id2" "Q4" 40
             , Questions "id3" "Q5" 50
             , Questions "id3" "Q5" 60
             ]

myfunc :: [AllQuestions] -> [Questions] -> [CallReportCSV] -> [CallReportCSV]
myfunc all questions oldCallsReports = do
  let testMerge :: Questions -> CallReportCSV -> Maybe CallReportCSV
      testMerge Questions{..} CallReportCSV{..} =
        if callId == callIdQU
          then Just $ CallReportCSV {questions = M.insert questionQU scoreQU questions, ..}
          else Nothing

  t <- questions
  take 1 $ mapMaybe (testMerge t) oldCallsReports

poop :: [CallReportCSV]
poop = myfunc allQuestions' questions' callReports'

main :: IO ()
main = print poop

--
-- Returns:
-- [ CallReportCSV {callId = "id1", agentDisplayName = "name1", questions = fromList [("Q1",10)]}
-- , CallReportCSV {callId = "id1", agentDisplayName = "name1", questions = fromList [("Q2",10)]}
-- , CallReportCSV {callId = "id1", agentDisplayName = "name1", questions = fromList [("Q3",10)]}
-- , CallReportCSV {callId = "id1", agentDisplayName = "name1", questions = fromList [("Q4",10)]}
-- , CallReportCSV {callId = "id1", agentDisplayName = "name1", questions = fromList [("Q5",10)]}
-- , CallReportCSV {callId = "id1", agentDisplayName = "name1", questions = fromList [("Q5",10)]}
-- ]
--
-- But expected:
-- [ CallReportCSV {callId = "id1", agentDisplayName = "name1", questions = fromList [("Q1",10), ("Q2",20)]}
-- , CallReportCSV {callId = "id1", agentDisplayName = "name1", questions = fromList [("Q1",30), ("Q2",40)]}
-- , CallReportCSV {callId = "id1", agentDisplayName = "name1", questions = fromList [("Q1",50), ("Q2",60)]}
-- ]
