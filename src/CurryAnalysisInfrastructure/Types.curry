module CurryAnalysisInfrastructure.Types where

import Text.Pretty (Doc)

data ModuleInformation
    = ModuleDocumentation Doc
    | ModuleSourceCode Doc
    | ModuleUnsafe Bool
    | ModuleExports [Export]
    | ModuleTypeclasses [Typeclass]
    | ModuleTypes [Type]
    | MoudleOperations [Operation]

data TypeInformation
    = TypeDocumentation Doc
    | TypeConstructors (Either External [Constructor])
    | TypeDefinition Doc

data TypeclassInformation
    = TypeclassDocumentation Doc
    | TypeclassMethods [Signature]
    | TypeclassDefinition Doc

data OperationInformation
    = OperationDocumentation Doc
    | OperationSourceCode Doc
    | OperationSignature Signature
    | OperationInfix Infix
    | OperationPrecedence Precedence
    | OperationDeterminism Determinism
    | OperationDemandness [Int]
    | OperationIndeterminism Bool
    | OperationSolutionCompleteness Bool
    | OperationTermination Bool
    | OperationTotallyDefined Bool

data Infix = Infix | InfixL | InfixR
    deriving (Read, Show)

type Precedence = Int

data Determinism = Det | NDet

type External = ()

type Constructor = String

type Operation = String

type Type = String

type Typeclass = String

type Export = String

data Signature 