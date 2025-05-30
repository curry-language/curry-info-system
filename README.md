CurryInfo: A tool to manage analysis and verification information about Curry packages
======================================================================================

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

This installs the executable `curry-info` which organizes the generation of
and access to various information about Curry module contained in packages.

Basic usage examples
--------------------

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

To show results about the operational behavior of operations,
`curry-info` uses other analysis and verification tools which exist for
Curry. If they are not installed, you cannot generate and access
this information, but only show some basic information about operations,
like their signature and definition as shown above.

In order to install the analysis and verification tools currently
supported and used by `curry-info`, run the following commands:

    > cypm install cass              # installs executable cass
    > cypm install verify-non-fail   # installs executable curry-calltypes

When `curry-info` is called without options, the list of all packages
currently stored in CurryInfo is shown:

    > curry-info
    packages: abstract-curry abstract-haskell addtypes ...


Web service / CGI mode
----------------------

If the option `--cgi` is given, `curry-info` is executed in CGI mode.
In this mode, the parameters are obtained from the value of the
environment variable `QUERY_STRING` which must contain all parameters
as URL-encoded strings separated by the character `&`.
The CGI mode is useful to maintain a web server containing
the infos about all packages. An example installation of such a server
can be accessed at URL

    https://cpm.curry-lang.org/curry-info/run.cgi

For instance, the following URL retrieves all versions of the package `base`:

    https://cpm.curry-lang.org/curry-info/run.cgi?-f0&--package=base&versions

The CGI mode and this server is used by the tool `cpm-query`
(see Curry package [cpm-query](https://cpm.curry-lang.org/pkgs/cpm-query.html))
to query information about Curry entities defined in a module of some
existing Curry package.


Installing a CurryInfo Web Application
--------------------------------------

In order to install a CurryInfo web server to be used as shown above, run

    > make install

This compiles the tools and install them in a local directory (see definition
of `WEBDIR` in the Makefile or run `make showconfig` to see its value).
Note that this should be done on the web server or a compatible machine.

After this local installation, run

    > make tar

to generate a tar file `WEBCURRYINFO.tgz` with the necessary contents of
`WEBDIR`. Now copy and unpack it in the desired directory of the web server
(e.g., `.../curry-info`).

Note that the cache of this installed web server is empty.
There are two options to fill the cache:

* Copy an [existing CurryInfo cache](https://cpm.curry-lang.org/curry-info/HTML/CURRYINFOCACHE.tgz)
  into the directory `.curry_info_cache` of the web server.

* Updating/generating analysis information for single package versions by

      > cpm-query --remote --generate <PACKAGE> <VERSION>

  or updating/generating analysis infos for all (newest compatible) package
  versions by

      > cpm-manage packagelist | cpm-query --remote --generate --from=-

  Note that these commands need to be executed on the host of the
  CurryInfo web server (see `CurryInfo.Options.managerAddrs`)
  since data changes are generally not allowed in CGI mode.

  Furthermore, `cpm-query` generates a configuration file `~/.cpmqueryrc`
  where one has to set the option `curryinfourl` to the URL of the
  CurryInfo web server.


HTML generation
---------------

If the option `--htmldir=DIR` is given, `curry-info` generates an HTML
representation of all entities stored in its cache in the directory `DIR`.
For instance, the representation of the entities stored in the
`curry-info` web service mentioned above can be accessed at URL

    https://cpm.curry-lang.org/curry-info/HTML

On the
[standard CurryInfo web server](https://cpm.curry-lang.org/curry-info/),
these web pages are nightly updated when new package versions arrive.


Technical notes
---------------

- The name of a module entity (operation, type, class) can also be
  an empty string. In this case, the analysis is performed
  (if the value of the `force` option is non-zero)
  but a result is not shown. This is useful for program analyses
  which compute at once information about all operations of a module,
  as the analysis and verification tools `cass` and `verify-non-fail`.
  For instance, the following command generates determinism information
  for all operations of the prelude:

      > curry-info -f2 -p base -x 3.3.0 -m Prelude -o '' cass-deterministic

- CurryInfo is designed as a generic and extensible system to collect and
  manage information about Curry entities. Thus, it is also possible to
  [add new information requests to CurryInfo](specifications/AddingRequests.md).

- The easiest way to use CurryInfo and the information stored in the central
  [CurryInfo server](https://cpm.curry-lang.org/curry-info/)
  is provided by the tool `cpm-query`
  (see Curry package [cpm-query](https://cpm.curry-lang.org/pkgs/cpm-query.html)).
  CurryInfo provides also a socket connection with a specific
  [server protocol](specifications/ServerProtocol.md).

Files
-----

CurryInfo maintains a file cache containing all currently available
analysis and verification information.
As a default, these files are stored in the directory `~/.curry_info_cache`.
This path is defined in `CurryInfo.Options.defaultCacheRoot` but
can be changed by the option `--cache`.

The contents of the cache directory has the following structure:

- `checkouts` (contains clones of packages checked out by `cypm checkout`)
    - *package1*-*version1*
    - *package2*-*version2*
    - ...
- `packages`
    - *pkg*
        - *pkg*.json
        - `versions`
            - *vsn*
                - *vsn*.json
                - `modules`
                    - *mod*
                        - *mod*.json
                        - `types`
                            - *type*.json
                        - `classes`
                            - *class*.json
                        - `operations`
                            - *op*.json

For instance, the information about the prelude operation `foldr1`
contained in version 3.3.0 of the base` package is stored in file

`.../packages/base/versions/3.3.0/modules/Prelude/operations/foldr1.json`

To speed up the access to CASS-requests for all operations,
CurryInfo also writes files named `ALL_<req>.txt` in `operations`
directories. These files contains the `CurryTerm` output format
of these requests.
