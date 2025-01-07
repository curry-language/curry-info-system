A tool to manage analysis and verification information about Curry packages
===========================================================================

This package contains the implementation of a tool to collect and
provide analysis and verification information about Curry modules
contained in Curry packages.
It can be used by other Curry tools which require such information,
like a REPL or a Curry Language Server.

Installation
------------

The tool itself can be simply installed by

    > git clone https://github.com/curry-language/curry-info-system.git
    > cd curry-info-system
    > cypm install

This install the executable `curry-info` which organizes the generation of
and access to various information about Curry module contained in packages.

Usage
-----

The following command outputs the list of
all operations defined in the module `Data.List` of package `base`
with version `3.3.0`:

    > curry-info -p base -x 3.3.0 -m Data.List operations

The following commands show the definitions of the type class `Ord`
and signature and operation `lines` (defined in the `Prelude` of
package `base` with version `3.3.0`), respectively:

    > curry-info -p base -x 3.3.0 -m Prelude -c Ord definition
    > curry-info -p base -x 3.3.0 -m Prelude -o lines signature
    > curry-info -p base -x 3.3.0 -m Prelude -o lines definition

The package and version options can be omitted if the module is part
of a package which can be found in the current load path.
For instance, the previous commands can be shortened as follows
(provided that the Curry system is based `base` package with version `3.3.0`):

    > curry-info -m Prelude -c Ord definition
    > curry-info -m Prelude -o lines signature
    > curry-info -m Prelude -o lines definition

To show more results about the analysis and verification of operations,
`curry-info` uses other analysis and verification tools which exists for
Curry. If they are not installed, you cannot generate and access
this information, but only show some basic information about operations,
like their signature and definition as shown above.

In order to install the analysis and verification tools currently
supported and used by `curry-info`, run the following commands:

    > cypm install cass              # installs executable cass
    > cypm install verify-non-fail   # installs executable curry-calltypes


Technical notes
---------------

- `curry-info` maintains a file cache with all available information.
  These files are stored in the directory `~/.curry_info_cache`
  (this constant is defined in `CurryInfo.Paths.getRoot`).


