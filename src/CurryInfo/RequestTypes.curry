------------------------------------------------------------------------------
--- This modules defines some types related to specify and implement
--- requests in CurryInfo.
------------------------------------------------------------------------------

module CurryInfo.RequestTypes where

import Data.List         ( intercalate )

import FlatCurry.Goodies ( funcRule, ruleArgs, ruleBody )
import FlatCurry.Pretty
import FlatCurry.Types   ( FuncDecl )
import Text.Pretty       ( Doc, pPrint )

------------------------------------------------------------------------------

--- Type for package names.
type Package = String

--- Type for package versions.
type Version = String

--- Type for package categories.
type Category = String

--- Type for references to a part of a file.
--- The file path is usually local to `getRoot`.
--- The further arguments are the first line of the slice and
--- the line after the slice to which this reference refers.
data Reference = Reference FilePath Int Int
  deriving (Show, Read)

--- Type for package dependencies
data Dependency = Dependency Package Disjunction
  deriving (Eq, Show, Read)

--- Type for disjunctions of version constraints.
type Disjunction = [Conjunction]

--- Type for conjunctions of version constraints.
type Conjunction = [VersionConstraint]

--- Type for version constraints.
data VersionConstraint
    = VExact Version
    | VGt Version
    | VLt Version
    | VGte Version
    | VLte Version
    | VMinCompatible Version
    | VMajCompatible Version
  deriving (Eq, Show, Read)

--- Type for module names.
type Module = String

--- Type for class names.
type Class = String

--- Type for class methods.
type Method = String

--- Type for data type names.
type Type = String

--- Type for constructors of a type.
type Constructor = String

--- Type for operation names.
type Operation = String

--- Type for signatures of operations.
type Signature = String

--- Type for request `infix`.
data Infix = Infix | InfixL | InfixR
  deriving (Read, Show)

--- Type for request `precedence`.
type Precedence = Int

------------------------------------------------------------------------------
--- An abstract term domain where terms are abstracted into their
--- top-level constructors.
--- `AAny` represents any expression, and
--- `ACons cs` a value rooted by some of the constructor `cs`.
data AType = ACons [(String,String)] | AAny
 deriving (Eq, Show, Read)

--- Shows an abstract value in prettier format.
prettyAType :: AType -> String
prettyAType AAny       = "_"
prettyAType (ACons cs) = "{" ++ intercalate "," (map snd cs) ++ "}"

--- The call type of some operation is either `Nothing` (always failing)
--- or `Just cts`, where `cts` is the list of abstract types
--- describing the conditions on arguments so that the operation does not fail.
type ACallType = Maybe [AType]

--- Shows an abstract call type (as computed by `curry-calltypes`)
--- in a prettier format.
showACallType :: ACallType -> String
showACallType Nothing = "<FAILING>"
showACallType (Just ats) = case ats of
  []   -> "()"
  [at] -> prettyAType at
  _    -> "(" ++ intercalate ", " (map prettyAType ats) ++ ")"

--- Pretty prints a FlatCurry function as a lambda expression.
showFuncDeclAsLambda :: FuncDecl -> String
showFuncDeclAsLambda fdecl =
  let rule = funcRule fdecl
  in '\\' : unwords (map (\i -> 'v' : show i) (ruleArgs rule)) ++
     " -> " ++ showExp (ruleBody rule)
 where
  showExp = pPrint . ppExp defaultOptions { qualMode = QualNone}

------------------------------------------------------------------------------

--- An InOutType is a disjunction, represented as a list,
--- of input/output type pairs.
--- It is parameterized over the abstract term domain.
data InOutType a = IOT [([a],a)]
  deriving (Eq, Show, Read)

--- Shows an `InOutType` in prettier syntax.
showIOT :: InOutType AType -> String
showIOT (IOT iots) = "{" ++ intercalate " || " (map showIOType iots) ++ "}"
 where
  showIOType (ict,oct) = "(" ++ intercalate "," (map prettyAType ict) ++ ")" ++
                         " |-> " ++ prettyAType oct

------------------------------------------------------------------------------
