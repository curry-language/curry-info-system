module CurryAnalysisInfrastructure.ErrorMessage where

import CurryAnalysisInfrastructure.Types

class ErrorMessage a where
    errorMessage :: a -> String

instance ErrorMessage CurryPackage where
    errorMessage (CurryPackage pkg) = "JSON file for package " ++ pkg ++ " could not be read."

instance ErrorMessage CurryVersion where
    errorMessage (CurryVersion pkg vsn) = "JSON file for version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

instance ErrorMessage CurryModule where
    errorMessage (CurryModule pkg vsn m) = "JSON file for module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

instance ErrorMessage CurryType where
    errorMessage (CurryType pkg vsn m t) = "JSON file for type " ++ t ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

instance ErrorMessage CurryTypeclass where
    errorMessage (CurryTypeclass pkg vsn m c) = "JSON file for typeclass " ++ c ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."

instance ErrorMessage CurryOperation where
    errorMessage (CurryOperation pkg vsn m o) = "JSON file for operation " ++ o ++ " of module " ++ m ++ " of version " ++ vsn ++ " of package " ++ pkg ++ " could not be read."