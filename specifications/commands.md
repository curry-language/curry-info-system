When a user requests specific information about something, this information is first looked up locally in the cache on the user's machine. If it is not in the cache, it will be requested from the server. If it is available on the server, the information will be send to the user. If it is not available, the server will generate that information and then send it to the user.

The arguments can be combined to get multiple information about a single entity. For example **/packages/*pkg*/version/*vsn*?categories?modules** return both lists in one json file with the fields *categories* and *modules* containing the lists.

---

## Get for packages

### /packages/*pkg*?versions

Return a list of versions given package has.

## Get for versions

### /packages/*pkg*/versions/*vsn*

Return the json file of given version of given package.

### /packages/*pkg*/versions/*vsn*?doc

Return the documentation of given version of given package.

### /packages/*pkg*/versions/*vsn*?categories

Return the list of categories of given version of given package.

### /packages/*pkg*/versions/*vsn*?modules

Return the list of modules defined in given version of given version.

## Get for modules

### /packages/*pkg*/versions/*vsn*/modules/*mod*

Return the json file of given module in given version of given package.

### /packages/*pkg*/versions/*vsn*/modules/*mod*?doc

Return the documentation of given module in given version of given package.

### /packages/*pkg*/versions/*vsn*/modules/*mod*?source

Return the source code of given module in given version of given package.

### /packages/*pkg*/versions/*vsn*/modules/*mod*?safe

Return for given module in given version of given package whether it is safe or not.

### /packages/*pkg*/versions/*vsn*/modules/*mod*?exports

Return a list of exports if given module in given version of given package.

### /packages/*pkg*/versions/*vsn*/modules/*mod*?types

Return a list of types defined in given module in given version of given package.

### /packages/*pkg*/versions/*vsn*/modules/*mod*?typeclasses

Return a list of typeclasses defined in given module in given version of given package.

### /packages/*pkg*/versions/*vsn*/modules/*mod*?operations

Return a list of operations defined in given module in given version of given package.

### /packages/*pkg*/versions/*vsn*/modules/*mod*?table

Return the table of available information about functions defined in given module in given version of given package.

## Get for types

### /packages/*pkg*/versions/*vsn*/modules/*mod*/types/*type*

Return the json file of given type.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/types/*type*?doc

Return the documentation of given type.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/types/*type*?constructors

Return the list of constructors of given type or that it is external.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/types/*type*?definition

Return the definition of given type.

## Get for typeclasses

### /packages/*pkg*/versions/*vsn*/modules/*mod*/typeclasses/*typeclass*

Return the json file of given typeclass.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/typeclasses/*typeclass*?doc

Return the documentation of given typeclass.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/typeclasses/*typeclass*?methods

Return a list of methods defined for given typeclass.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/typeclasses/*typeclass*?definition

Return the definition of given typeclass.

## Get for operations

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operations/*op*

Return the json file of given function.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operations/*op*?doc

Return the documentation of given function.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operations/*op*?source

Return the source code of given function.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operations/*op*?signature

Return the signature of given function.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operations/*op*?determinism

Return whether given function is deterministic or nondeterministic.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operations/*op*?demandness

Return a list of arguments given function demands.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operations/*op*?indeterminism

Return whether given function is indeterministic.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operations/*op*?solution_complete

Return whether given function is solution complete or not.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operations/*op*?termination

Return whether given function is always terminating or not.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operations/*op*?totally_defined

Return whether given function is totally defined or not.

## Get for operators

### SAME AS FUNCTIONS

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operators/*op*?infix

Return what kind of infix operator the given operator is.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operators/*op*?precedence

Return what precedence the given operator has.

### /packages/*pkg*/versions/*vsn*/modules/*mod*/operators/*op*?nondeterminism 

Return whether the given operator encapsulates or produces non-deterministic values.

---

## Queries for packages

### /?packages

Return a list of available packages.

### /packages/*pkg*?information

Returns a list of available information for given package. Also return a list of information that can be generated.

## Queries for versions

### /packages/*pkg*/versions/*vsn*?information

Return a list of available information for given version of given package. Also return a list of information that can be generated.

## Queries for modules

### /packages/*pkg*/versions/*vsn*/modules/*mod*?information

Return a list of available information for given module in given version of given package. Also return a list of information that can be generated.

## Queries for classes

### /packages/*pkg*/versions/*vsn*/modules/*mod*/classes/*typeclass*?information

Return a list of available information for given type class of given module in given version of given package. Also return a list of information that can be generated.

## Queries for types

### /packages/*pkg*/versions/*vsn*/modules/*mod*/types/*type*?information

Return a list of available information for given type of given module in given version of given package. Also return a list of information that can be generated.

## Queries for operations

### /packages/*pkg*/versions/*vsn*/modules/*mod*/function/*op*?information

Return a list of available information for given function of given module in given version of given package. Also return a list of information that can be generated.
