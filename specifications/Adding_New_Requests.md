Adding Information Requests to CurryInfo
========================================

This document explains the process of how to add new requests to CurryInfo.
Since the whole program is implemented using Curry,
new request need to be implemented in Curry (see below).

Each request returns a piece of information about an object of a Curry
package and also saves this in a JSON file.
The possible objects are Package, Version, Module, Type, Class and
Operation. Each object has its own type (see module `CurryInfo.Types`)
with components to specify the exact object.
For example, a module object is specified by a value of type `CurryModule`
which has three string components, one being the name of the package,
another being the version number, and the last being the name
of the module itself.

When a new request should be added, one has to register it in the module
`CurryInfo.Configuration`. For every type of object, there is a separate
configuration in the form of a list. For instance, the constant 
`operationConfiguration` contains all requests to get information about an 
operation defined in some module of a package.
To register a new request, one has to add a new element by using
the request generator operation

```
registerRequest :: ConvertJSON b -- to store infos in JSON format
                => String        -- request name/identifier
                -> String        -- short help description
                -> Generator a b -- generator of requested information
                -> Printer b     -- returns a user-friendly version of a result
                -> RegisteredRequest a
```

Thus, one has to provide two strings and two operations in order
to implement a new request.
The request is generic w.r.t. two types:

* `a` is the type of the object for which this request computes some
  information, like, `CurryPackage` for a package, `CurryType` for a
  type defined in a module, or `CurryOperation` for an operation
  defined in a Curry module.
* `b` is the type of results returned by the request, typically
  some standard type (e.g., `String`) or some type defined by the
  implementor of the request.

Since CurryInfo stores all information in JSON format, the result
of the request must be convertible into JSON data, as required
by the type constraint `ConvertJSON b` (see module `JSON.Convert`
of packate `json` for auxiliary conversion operations).

The first two arguments of `registerRequest` are the name and a short
description of the request. The name is used as an argument when running
the tool to specify the kind of information to be computed.
Note that the name `_realname_` is used for internal purposes
and should not be used as a regular request.
The description is the explanation of this request which is shown
to the user when they use the help option of the tool.

The two remaining arguments of `registerRequest` are the actual implementation
of this request and their connection to the CurryInfo tool.
The first of these arguments is the most important one, as it will generate
the information to be returned by the request.
Its type is

```
type Generator a b = Options -> a -> IO (Maybe b)
```

The argument of type `Options` (defined in `CurryInfo.Types`) supports
to access the options with which the user runs the tool.
For instance, one can print specific messages depending on the verbosity level
(see module `CurryInfo.Verbosity` for some helper functions w.r.t. verbosity). 
As the generation of the requested information may fail and it usually
requires to access the outside world, like the file system, the result of
a generator is of type `IO (Maybe b)`.

The fourth argument of `registerRequest` is of type

```
type Printer b = Options -> b -> IO String
```

Similarly to the operation generating a requested result,
the `Options` parameter allows to print messages to inform the user
about the progress. This operation is used to create the string that is shown
to the user as result at the end as output.
The reason to use a separate operation (instead of `show`) is because the
stored result might not contain the actual information the user wants
to see.
For instance, there are some requests that store the result as references
to some section of a file instead of storing the section directly.
In this case the printing operation needs to extract that section
and present it to the user. This is also the reason why this operation
is an I/O action, as it might need to access the environment,
like specific files.

If all these the operations are implemented for a specific request,
the last thing to be done is to register this request in the module
`CurryInfo.Configuration`. For this purpose, the corresponding
configuration (i.e., `versionConfiguration` for package versions or
`typeclassConfiguration` for type classes) needs to be extended
by defining the new request with the use of the operation `registerRequest`,
as described above.
When this is done, the CurryInfo tool must be recompiled.
After that one should see with the `--help` option the new request
in the list of all requests.
