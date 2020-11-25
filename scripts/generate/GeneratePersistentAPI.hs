{-
  stack script --resolver lts-16.23
    --package aeson
    --package containers
    --package mustache
    --package text
    --package unliftio
    --package yaml
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (forM_)
import Data.Aeson (FromJSON(..), withObject, (.!=), (.:), (.:?))
import Data.Char (isAlphaNum, toUpper)
import Data.List (nub, sort)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import Text.Mustache (ToMustache(..), object, (~>))
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Types as Mustache

{- Configuration -}

-- | See `README.md` for a description of this data type.
data PersistentFunction = PersistentFunction
  { name        :: Text
  , constraints :: [Text]
  , args        :: [Text]
  , result      :: Text
  , condition   :: Maybe Text
  } deriving (Show)

instance FromJSON PersistentFunction where
  parseJSON = withObject "PersistentFunction" $ \o ->
    PersistentFunction
      <$> o .: "name"
      <*> o .:? "constraints" .!= []
      <*> o .:? "args" .!= []
      <*> o .: "result"
      <*> o .:? "condition"

{- Rendering -}

data Context = Context
  { functions :: [FunctionContext]
  }

instance ToMustache Context where
  toMustache Context{..} = object [ "functions" ~> functions ]

buildContext :: [PersistentFunction] -> Context
buildContext = Context . map buildFunctionContext

data FunctionContext = FunctionContext
  { name        :: Text
  , constraints :: [Text]
  , args        :: [Text]
  , result      :: Text
  , condition   :: Maybe Text
  }

buildFunctionContext :: PersistentFunction -> FunctionContext
buildFunctionContext PersistentFunction{..} = FunctionContext{..}

instance ToMustache FunctionContext where
  toMustache FunctionContext{..} = object
    [ "name" ~> name
    , "nameCapital" ~> capitalize name
    , "constraints" ~> enumerateWith fromConstraint constraints
    , "args" ~> enumerateWith fromArg args
    , "result" ~> result
    , "sqlQueryRepRecord" ~> sqlQueryRepRecord
    , "recordTypeVars" ~> recordTypeVars
    , "withCondition" ~> \stree -> pure @Mustache.SubM $
        case condition of
          Nothing -> stree
          Just cond -> concat
            [ [Mustache.TextBlock $ "#if " <> cond <>"\n"]
            , stree
            , [Mustache.TextBlock "#endif\n"]
            ]
    ]
    where
      fromConstraint constraint = object [ "type" ~> constraint ]
      fromArg arg = object [ "type" ~> arg ]
      recordTypeVars = getRecordTypeVars constraints

      -- the `record` type variable for SqlQueryRep
      sqlQueryRepRecord = case recordTypeVars of
        [] -> "Void"
        [record] -> record
        records -> "(" <> Text.intercalate ", " records <> ")"

-- | Get all `record` type variables in the given list of constraints.
--
-- A type variable is considered a `record` type variable if it matches the
-- pattern "record|recordN", where `N` is a number. The type variables will
-- be sorted when returned.
--
-- e.g.
--   ["Foo record"] -> ["record"]
--   ["Bar record1", "Foo record2 record1"] -> ["record1", "record2"]
getRecordTypeVars :: [Text] -> [Text]
getRecordTypeVars = sort . nub . concatMap getPersistRecordsIn
  where
    getPersistRecordsIn = filter ("record" `Text.isPrefixOf`) . Text.words . ignoreNonAlphaNum
    ignoreNonAlphaNum = Text.map (\c -> if isAlphaNum c then c else ' ')

capitalize :: Text -> Text
capitalize t = case Text.uncons t of
  Just (c, cs) -> Text.cons (toUpper c) cs
  Nothing -> t

-- | Convert each element in the list into a Value with the given function,
-- adding the "index" and "last" keys indicating the element's index in the list
-- and whether the element is the last one in the list, respectively.
enumerateWith :: (a -> Mustache.Value) -> [a] -> [Mustache.Value]
enumerateWith f xs =
  let mkElem x i = merge (f x) $ object
        [ "index" ~> i
        , "last" ~> (i == length xs)
        ]
  in zipWith mkElem xs [1..]

-- If only Value had a Monoid instance...
merge :: Mustache.Value -> Mustache.Value -> Mustache.Value
merge (Mustache.Object o1) (Mustache.Object o2) = Mustache.Object $ o1 <> o2
merge v _ = v

{- Main -}

main :: IO ()
main = do
  context <- buildContext <$> Yaml.decodeFileThrow "persistent-api.yaml"

  generate "SqlQueryRep.mustache" "Database/Persist/Monad/SqlQueryRep.hs" context
  generate "Shim.mustache" "Database/Persist/Monad/Shim.hs" context

srcDir :: FilePath
srcDir = "../../src/"

generate :: ToMustache k => FilePath -> FilePath -> k -> IO ()
generate templatePath output value = do
  template <- either (error . show) return =<< Mustache.automaticCompile ["./templates"] templatePath
  case Mustache.checkedSubstitute template value of
    ([], rendered) -> Text.writeFile (srcDir ++ output) rendered
    (errors, _) -> error $ unlines $
      "Found errors when generating template:" : map showError errors
  where
    showError e = "* " ++ show e
