A tool to manage analysis and verification information about Curry packages
===========================================================================

This package contains the implementation of a tool to collect and
provide analysis and verification information about Curry modules
contained in Curry packages.
It can be used by other Curry tools which require such information,
like a REPL or a Curry Language Server.

Technical notes
---------------

- Cache files are stored in `~/.curry_info_cache` (this constant is
  defined in `CurryInfo.Paths.root`)

