------------------------------------------------------------------------------
--- This modules defines some types occurring as results of various requests.
------------------------------------------------------------------------------

module CurryInfo.RequestTypes where

import Data.List ( intercalate )

------------------------------------------------------------------------------
data Infix = Infix | InfixL | InfixR
  deriving (Read, Show)

type Precedence = Int

type Version = String

type Package = String

type Category = String

type Method = String

data Reference = Reference String Int Int
  deriving (Show, Read)

-- Package dependency

data VersionConstraint
    = VExact Version
    | VGt Version
    | VLt Version
    | VGte Version
    | VLte Version
    | VMinCompatible Version
    | VMajCompatible Version
  deriving (Eq, Show, Read)

type Conjunction = [VersionConstraint]

type Disjunction = [Conjunction]

data Dependency = Dependency Package Disjunction
  deriving (Eq, Show, Read)

--- An abstract term domain where terms are abstracted into their
--- top-level constructors.
--- `AAny` represents any expression, and
--- `ACons cs` a value rooted by some of the constructor `cs`.
data AType = ACons [(String,String)] | AAny
 deriving (Eq, Show, Read)

-- Shows an abstract value in prettier format.
prettyAType :: AType -> String
prettyAType AAny       = "_"
prettyAType (ACons cs) = "{" ++ intercalate "," (map snd cs) ++ "}"

------------------------------------------------------------------------------