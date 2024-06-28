module CurryAnalysisInfrastructure.Writer where

import CurryAnalysisInfrastructure.Types
import CurryAnalysisInfrastructure.JPretty (json, JPretty)
import CurryAnalysisInfrastructure.Paths (getJSONPath, Path)

import JSON.Pretty (ppJSON)

type Writer a b = a -> [b] -> IO ()

writeInformation :: (Path a, JPretty b) => Writer a b
writeInformation input infos = do
    let jtext = (ppJSON . json) infos
    path <- getJSONPath input
    writeFile path jtext

{-
writePackageInformation :: Writer CurryPackage PackageInformation
writePackageInformation x@(CurryPackage pkg) information = do
    let jtext = (ppJSON . json) information
    path <- getJSONPath x
    writeFile path jtext

writeVersionInformation :: Writer CurryVersion VersionInformation
writeVersionInformation x@(CurryVersion pkg vsn) information = do
    let jtext = (ppJSON . json) information
    path <- getJSONPath x
    writeFile path jtext

writeModuleInformation :: Writer CurryModule ModuleInformation
writeModuleInformation x@(CurryModule pkg vsn m) information = do
    let jtext = (ppJSON . json) information
    path <- getJSONPath x
    writeFile path jtext
-}