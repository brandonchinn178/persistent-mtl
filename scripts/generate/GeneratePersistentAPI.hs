{-
  stack script --resolver lts-19.20
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

import Control.Monad (forM_)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Char (isAlphaNum, toUpper)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import Text.Mustache (ToMustache (..), object, (~>))
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Types as Mustache

{- Configuration -}

-- | See `README.md` for a description of this data type.
data PersistentFunction = PersistentFunction
  { name :: Text
  , constraints :: [Text]
  , args :: [Text]
  , result :: Text
  , condition :: Maybe Text
  , conduitFrom :: Maybe Text
  }
  deriving (Show)

instance FromJSON PersistentFunction where
  parseJSON = withObject "PersistentFunction" $ \o ->
    PersistentFunction
      <$> (o .: "name")
      <*> (o .:? "constraints" .!= [])
      <*> (o .:? "args" .!= [])
      <*> (o .: "result")
      <*> (o .:? "condition")
      <*> (o .:? "conduitFrom")

{- Rendering -}

data Context = Context
  { functions :: [FunctionContext]
  }

instance ToMustache Context where
  toMustache Context{..} =
    object
      [ "functions" ~> enumerate functions
      , "sqlQueryRepConstructors" ~> enumerate (filter shouldGenerateSqlQueryRep functions)
      ]

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
  { name :: Text
  , constraints :: [Text]
  , args :: [Text]
  , result :: Text
  , condition :: Maybe Text
  , conduitFrom :: Maybe FunctionContext
  }

instance ToMustache FunctionContext where
  toMustache FunctionContext{..} =
    object
      [ "name" ~> name
      , "nameCapital" ~> capitalize name
      , "constraints" ~> enumerateWith fromConstraint constraints
      , "args" ~> enumerateWith fromArg args
      , "sqlQueryRepResult" ~> result
      , "sqlQueryRepRecord" ~> sqlQueryRepRecord
      , "recordTypeVars" ~> recordTypeVars
      , "result" ~> if hasConduitFrom then result else "m " <> result
      , "withCondition"
          ~> Mustache.overText
            ( \t ->
                case condition of
                  Nothing -> t
                  Just cond ->
                    Text.concat
                      [ "#if " <> cond <> "\n"
                      , t
                      , "#endif\n"
                      ]
            )
      , "conduitFrom?" ~> hasConduitFrom
      , "conduitFrom" ~> conduitFrom
      , -- testing
        "sqlQueryRepExample"
          ~> let modifyType =
                  replaceAll
                    [ ("record", "Person")
                    , ("record1", "Person")
                    , ("record2", "Post")
                    , ("m2", "IO")
                    , ("[a]", "[Entity Person]")
                    ]
              in modifyType $ Text.unwords ["SqlQueryRep", sqlQueryRepRecord, result]
      ]
    where
      fromConstraint constraint = object ["type" ~> constraint]
      fromArg arg = object ["type" ~> arg]
      recordTypeVars = getRecordTypeVars constraints
      hasConduitFrom = isJust conduitFrom

      -- the `record` type variable for SqlQueryRep
      sqlQueryRepRecord = case recordTypeVars of
        [] -> "Void"
        [record] -> record
        records -> "(" <> Text.intercalate ", " records <> ")"

shouldGenerateSqlQueryRep :: FunctionContext -> Bool
shouldGenerateSqlQueryRep FunctionContext{..} = isNothing conduitFrom

{-| Get all `record` type variables in the given list of constraints.

 A type variable is considered a `record` type variable if it matches the
 pattern "record|recordN", where `N` is a number. The type variables will
 be sorted when returned.

 e.g.
   ["Foo record"] -> ["record"]
   ["Bar record1", "Foo record2 record1"] -> ["record1", "record2"]
-}
getRecordTypeVars :: [Text] -> [Text]
getRecordTypeVars = sort . nub . concatMap getPersistRecordsIn
  where
    getPersistRecordsIn = filter ("record" `Text.isPrefixOf`) . Text.words . ignoreNonAlphaNum
    ignoreNonAlphaNum = Text.map (\c -> if isAlphaNum c then c else ' ')

capitalize :: Text -> Text
capitalize t = case Text.uncons t of
  Just (c, cs) -> Text.cons (toUpper c) cs
  Nothing -> t

replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll replacers haystack = foldr (uncurry Text.replace) haystack replacers

{-| Add to each element keys that indicate information about the element's
 index in the list.
-}
enumerate :: Mustache.ToMustache a => [a] -> [Mustache.Value]
enumerate xs =
  let mkElem x i =
        merge (Mustache.toMustache x) $
          object
            [ "index" ~> i
            , "first" ~> (i == 1)
            , "last" ~> (i == length xs)
            ]
   in zipWith mkElem xs [1 ..]

{-| Convert each element in the list into a Value with the given function,
 then enumerate each element.
-}
enumerateWith :: (a -> Mustache.Value) -> [a] -> [Mustache.Value]
enumerateWith f = enumerate . map f

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
  generate context "SqlShim.mustache" $ root ++ "src/Database/Persist/Sql/Shim.hs"
  generate context "TestHelpers.mustache" $ root ++ "test/Generated.hs"

generate :: ToMustache k => k -> FilePath -> FilePath -> IO ()
generate context templatePath output = do
  template <- either (error . show) return =<< Mustache.automaticCompile ["./templates"] templatePath
  case Mustache.checkedSubstitute template context of
    ([], rendered) -> Text.writeFile output rendered
    (errors, _) ->
      error . unlines $
        "Found errors when generating template:" : map showError errors
  where
    showError e = "* " ++ show e
