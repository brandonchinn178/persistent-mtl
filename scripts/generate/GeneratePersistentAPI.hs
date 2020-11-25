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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (forM_)
import Data.Aeson (FromJSON(..), withObject, (.!=), (.:), (.:?))
import Data.Char (isAlphaNum, toUpper)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
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
  , conduitFrom :: Maybe Text
  } deriving (Show)

instance FromJSON PersistentFunction where
  parseJSON = withObject "PersistentFunction" $ \o ->
    PersistentFunction
      <$> o .: "name"
      <*> o .:? "constraints" .!= []
      <*> o .:? "args" .!= []
      <*> o .: "result"
      <*> o .:? "condition"
      <*> o .:? "conduitFrom"

{- Rendering -}

data Context = Context
  { functions :: [FunctionContext]
  }

instance ToMustache Context where
  toMustache Context{..} = object [ "functions" ~> functions ]

buildContext :: [PersistentFunction] -> Context
buildContext functions = Context functionContexts
  where
    functionContexts = map buildFunctionContext functions

    functionContextMap = Map.fromList $ map (\ctx@FunctionContext{name} -> (name, ctx)) functionContexts
    getFunctionContext name =
      fromMaybe (error $ Text.unpack $ "Could not find function named: " <> name) $
        Map.lookup name functionContextMap

    buildFunctionContext PersistentFunction{..} =
      FunctionContext
        { conduitFrom = getFunctionContext <$> conduitFrom
        , ..
        }

data FunctionContext = FunctionContext
  { name        :: Text
  , constraints :: [Text]
  , args        :: [Text]
  , result      :: Text
  , condition   :: Maybe Text
  , conduitFrom :: Maybe FunctionContext
  }

instance ToMustache FunctionContext where
  toMustache FunctionContext{..} = object
    [ "name" ~> name
    , "nameCapital" ~> capitalize name
    , "constraints" ~> enumerateWith fromConstraint constraints
    , "args" ~> enumerateWith fromArg args
    , "sqlQueryRepResult" ~> result
    , "sqlQueryRepRecord" ~> sqlQueryRepRecord
    , "recordTypeVars" ~> recordTypeVars
    , "result" ~> if hasConduitFrom then result else "m " <> result
    , "withCondition" ~> \stree -> pure @Mustache.SubM $
        case condition of
          Nothing -> stree
          Just cond -> concat
            [ [Mustache.TextBlock $ "#if " <> cond <>"\n"]
            , stree
            , [Mustache.TextBlock "#endif\n"]
            ]
    , "generateSqlQueryRep?" ~> not hasConduitFrom
    , "conduitFrom?" ~> hasConduitFrom
    , "conduitFrom" ~> conduitFrom
    ]
    where
      fromConstraint constraint = object [ "type" ~> constraint ]
      fromArg arg = object [ "type" ~> arg ]
      recordTypeVars = getRecordTypeVars constraints
      hasConduitFrom = isJust conduitFrom

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

  let root = "../../"

  generate context "SqlQueryRep.mustache" $ root ++ "src/Database/Persist/Monad/SqlQueryRep.hs"
  generate context "Shim.mustache" $ root ++ "src/Database/Persist/Monad/Shim.hs"

generate :: ToMustache k => k -> FilePath -> FilePath -> IO ()
generate context templatePath output = do
  template <- either (error . show) return =<< Mustache.automaticCompile ["./templates"] templatePath
  case Mustache.checkedSubstitute template context of
    ([], rendered) -> Text.writeFile output rendered
    (errors, _) -> error $ unlines $
      "Found errors when generating template:" : map showError errors
  where
    showError e = "* " ++ show e
