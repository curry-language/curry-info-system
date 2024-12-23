Adding Information Requests to CurryInfo
========================================

This section explains the process of how to implement your own requests
to CurryInfo. As the whole program is implemented using Curry,
your additions also need to be implemented in Curry as you need
to edit the existing code.

Each request returns a piece of information about an object of a Curry
Package and also saves this in a json file on the local machine. The
objects are Package, Version, Module, Type, Typeclass and
Operation. Each object has its own type with the names needed to find
the exact object. For example a Module object would have three
strings, one being the name of the package, another being the version
number and the last being the name of the module itself.

When you wish to add your own request, you have to register it in the module
`CurryInfo.Configuration`. For every type of object there is a separate
configuration in the form of a list. For instance, the constant 
`operationConfiguration` contains all request to get information about an 
operation in some module. To register a new request, you have to add a new 
element by using the request generator operation

```
registerRequest :: ConvertJSON b -- to store infos in JSON format
                => String        -- request name/identifier
                -> String        -- short help description
                -> Generator a b -- generator of requested information
                -> Printer b     -- returns a user-friendly version of a result
                -> RegisteredRequest a
```

Thus, one has to provide two strings and four operations in order
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
of request must be convertible into JSON data, as required
by the type constraint `ConvertJSON b` (see module `JSON.Convert`
of packate `json` for auxilary conversion operations).

The first two arguments of `registerRequest` are the name and a short
description of the request. The name is used as an argument when running
the tool to specify the kind of information to be computed.
Note that the name `_realname_` is used for internal purposes
and should not be used as a regular request.
The description is the explanation of this request which is shown
to the user when they use the help option of the tool.

The further arguments of `registerRequest` are the actual implementation
of this request and their connection to the CurryInfo tool.
The first of these arguments is the most important one, as it will generate
the information to be returned by your request.
Its type is
```
type Generator a b = Options -> a -> IO (Maybe b)
```
The argument of type `Options` (defined in `CurryInfo.Types`) supports
to access the options with which the user runs the tool.
For instance, one can print specific messages depending on the verbosity level
(see module `CurryInfo.Verbosity` for some helper functions w.r.t. verbosity). 
As the generating of the information may fail and it usually requires
access the outside world, like the file system, the result of
a generator is of type `IO (Maybe b)`.

The fourth argument of `registerRequest` is of type
```
type Printer b = Options -> b -> IO String
```
Similarly to the operation generating a requested result,
the `Options` parameter allows to print messages to inform the user
about the progress. This operation is used to create the string that is shown
to the user as result at the end as output.
The reason, for why a separate operation is needed, is because the
stored result might not contain the actual information the user wants
to see.
For instance, there are some requests that store the result as references
to a section of a file instead of storing the section directly.
In this case the printing operation needs to extract that section
and present it to the user. This is also the reason why this operation
is an I/O action, as it might need to access the environment,
like specific files.

If all these the operations are implemented for a specific request,
the last thing to be done is to register this request in the module
`CurryInfo.Configuration`. Go into the corresponding configuration (i.e.,
`versionConfiguration` for package versions or `typeclassConfiguration`
for type classes) and use the operation `registerRequest` described above
to add this request.
When you have done that, you need to recompile the tool.
After that, to make sure your request is correctly registered,
you can run the tool with the help option and see that your request
is in the correct list with its name and description.
You should now be able to run the tool with your new request.
