#!/usr/bin/env stack
{- stack runghc --package Cabal -}

import Data.List (intercalate)
import Distribution.Package (packageVersion)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Version (versionNumbers)

main :: IO ()
main = do
  packageDesc <- readGenericPackageDescription Verbosity.silent "persistent-mtl.cabal"
  let version = intercalate "." . map show . versionNumbers . packageVersion $ packageDesc
  -- https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#setting-an-output-parameter
  putStrLn $ "version=" ++ version
