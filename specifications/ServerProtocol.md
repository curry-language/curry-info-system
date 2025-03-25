# Server Commands

When `curry-info` is started with option `--server`,
it runs in server mode, i.e., one can communicate with `curry-info`
via sockets. To specify the communication port, use option `--port=N`.
If this option is not used, `curry-info` selects a free port
which is shown after starting the server.
The commands which can be used to communicate with the server
are described below.

* GetRequests *obj?*
* GetCommands
* RequestPackageInformation *outform* *force* *pkg* *[reqs]*
* RequestVersionInformation *outform* *force* *pkg* *vsn* *[reqs]*
* RequestModuleInformation *outform* *force* *pkg* *vsn* *mod* *[reqs]*
* RequestTypeInformation *outform* *force* *pkg* *vsn* *mod* *type* *[reqs]*
* RequestClassInformation *outform* *force* *pkg* *vsn* *mod* *class* *[reqs]*
* RequestOperationInformation *outform* *force* *pkg* *vsn* *mod* *op* *[reqs]*
* RequestAllTypesInformation *outform* *force* *pkg* *vsn* *mod* *[reqs]*
* RequestAllClassesInformation *outform* *force* *pkg* *vsn* *mod* *[reqs]*
* RequestAllOperationsInformation *outform* *force* *pkg* *vsn* *mod* *[reqs]*
* StopServer

Output formats are: text for plain text, json for a json value and CurryTerm for a list of curry terms.

Force options are: 0 for only lookup, 1 for lookup and generating if necessary, 2 for always generating.

## GetRequests *obj?*

When you send this message to the server, it responds with lists of all available requests you can do for each kind of object. Alternatively you can also name a specific kind of object to only get the list of possible requests for that. The kinds of objects are Package, Version, Module, Type, Typeclass and Operation.

### Examples

* GetRequests           - Returns all requests of all kinds of objects
* GetRequests Package   - Returns all requests you can do for packages
* GetRequests Type      - Returns all requests you can do for types

## GetCommands

When you send this message to the server, it answers with the list of avaible messages you can send to the server, including this one.

## RequestPackageInformation *outform* *force* *pkg* *[reqs]*

When you send this message to the server, you have to also add the output format of the answer and the force option you want to be used. For the object you need to add the package. Lastly, you add the list of requests you want to do.

### Example

* RequestPackageInformation json 0 directory versions   - Returns the list of versions of the package 'directory' if they are available using json as output format.

## RequestVersionInformation *outform* *force* *pkg* *vsn* *[reqs]*

When you send this message to the server, you have to also add the output format of the answer and the force option you want to be used. For the object you need to add the package and the version. Lastly, you add the list of requests you want to do.

### Example

* RequestVersionInformation text 1 socket 3.0.0 categories modules  - Returns the list of categories and modules the package 'socket' with version '3.0.0' as text. When the information is not available yet, it is generated on the spot.

## RequestModuleInformation *outform* *force* *pkg* *vsn* *mod* *[reqs]*

When you send this message to the server, you have to also add the output format of the answer and the force option you want to be used. For the object you need to add the package, the version and the module. Lastly, you add the list of requests you want to do.

### Example

* RequestModuleInformation CurryTerm 2 base 3.2.0 Control.Monad types cass-unsafemodule - Returns the list of types and the analysis result of whether the module is unsafe for the module 'Control.Monad' of package 'base' with version '3.2.0' as a list of curry terms. The information is generated in this case, whether the information is already available or not.

## RequestTypeInformation *outform* *force* *pkg* *vsn* *mod* *type* *[reqs]*

When you send this message to the server, you have to also add the output format of the answer and the force option you want to be used. For the object you need to add the package, the version, the module and the type. Lastly, you add the list of requests you want to do.

### Example

* RequestTypeInformation text 0 json 3.0.0 JSON.Data JValue constructors    - Returns the list of constructors for the type 'JValue' using text as output format. The information is only looked up, not generated if it is missing.

## RequestClassInformation *outform* *force* *pkg* *vsn* *mod* *class* *[reqs]*

When you send this message to the server, you have to also add the output format of the answer and the force option you want to be used. For the object you need to add the package, the version, the module and the type class. Lastly, you add the list of requests you want to do.

### Example

* RequestTypeclassInformation json 1 base 3.2.0 Prelude Eq methods  - Returns the list of methods of the typeclass 'Eq' using json as output format. The information is generated on the spot, if it is missing.

## RequestOperationInformation *outform* *force* *pkg* *vsn* *mod* *op* *[reqs]*

When you send this message to the server, you have to also add the output format of the answer and the force option you want to be used. For the object you need to add the package, the version, the module and the operation. Lastly, you add the list of requests you want to do.

### Example

* RequestOperationInformation CurryTerm 0 socket 3.0.0 Network.Socket cass-deterministic connectToSocket signature  - Returns the analysis result of whether the operation is deterministic and the signature of the operation 'connectToSocket' using curry terms as output format. The information is only looked up, not generated if it is missing.

## RequestAllTypesInformation *outform* *force* *pkg* *vsn* *mod* *[reqs]*

When you send this message to the server, it is equivalent to requesting the same information for all types in the module. Therefore, you don't need to name a type, as the same requests are processed for all types found in the given module.

## RequestAllClassesInformation *outform* *force* *pkg* *vsn* *mod* *[reqs]*

When you send this message to the server, it is equivalent to requesting the same information for all type classes in the module. Therefore, you don't need to name a type, as the same requests are processed for all types found in the given module.

## RequestAllOperationsInformation *outform* *force* *pkg* *vsn* *mod* *[reqs]*

When you send this message to the server, it is equivalent to requesting the same information for all operations in the module. Therefore, you don't need to name a type, as the same requests are processed for all types found in the given module.

## StopServer

When you send this message, the server stops itself and the connection is closed.