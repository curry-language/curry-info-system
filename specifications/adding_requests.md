- Create new constructor
- Create field name in instance in Types
- Create case in JPretty
- Create case in JParser
- Create printer in Printer
- Create generator in Generator
- Register in configuration (description, generator, printer)


This section explains the process of how to implement your own requests to CurryInfo. As the whole program is implemented using Curry, your additions also need to be implemented in Curry as you need to edit the existing code.

Each request returns a piece of information about an object of a Curry Package and also saves this in a json file on the local machine. The objects are Package, Version, Module, Type, Typeclass and Operation. Each object has its own type with the names needed to find the exact object. For example a Module object would have three strings, one being the name of the package, another being the version number and the last being the name of the module itself.

Each object has its own type for the information, for example 'PackageInformation'. In this type, every piece of information that can be requested has its own constructor with the information as its components. You will need to add your own constructor to the corresponding information type. All important types can be found in CurryInfo.Types.

When you wish to add your own request, you have to register it in CurryInfo.Configuration. For every type of object there is its own configuration in the form of a list. A request needs several parts you need to deliver.

The first two are the request string and the description string. The request string is the one the user can use to initiate your request. The description string is what is show if the help option is used when running the program.

Next you need to implement an operation that will generate the information of your request. This operation takes two inputs, one being the options set by the user and the other being the object type for which the user requests the information. As to generate the information the program may need to access the outside world (for example to read a file) and as the generation can fail for some reason, the operation returns its result in the IO monad in the form of the Maybe monad. The result itself should be your added constructor with the information generated in the operation. The already implemented generators can be found in Curry.Info.Generator.

Then you need another operation that will create an output for your request, that is shown to the user (as a string, as a json file, etc.). It also takes two arguments, the first being the options again and the other being the information of your request. Just like the generating operation, this printing operation may also need to access the outside world and this may also fail. So it also returns its result in the IO monad in the form of the Maybe monad, only this time as a string. As the result is supposed to be used by the user (or sent to another machine without access to the local files), the access to the outside world might be necessary to use information, that is not saved in the json file. For example the request 'documentation' for an operation would be saved as a reference in the json file, but in the printing operation would take the corresponding section and use that as output. The already implemented printers can be found in Curry.Printer.

Now you will need to edit some instances of typeclasses, where your added constructor needs to be added as a case. The first is in CurryInfo.JPretty the typeclass JPretty. This typeclass has the method 'jpretty', which returns the field for the request that will be saved in the json file. You will need to edit the instance of the information type, to which you added the new constructor. It takes the information type as input and returns both a string (the field name) and a JValue (the field value, see the package 'json' for more about this). You only need to convert the information of your request in a way, that makes it easy to reconstruct it when reading the file.

The next typeclass is JParser in CurryInfo.JParser with the method 'jparseField'. It is the inverse of the previous method 'jpretty'. As such, it takes the string and the JValue to return your request's information. Because that information might be missing (for example if it is the first time the request is used) or the parsing might fail for some reason, it returns the information in the Maybe monad. Make sure it matches your editing of 'jpretty'.

The last typeclass you need to edit is Field in CurryInfo.Types. It's method 'fieldName' takes the information type for which it is instantiated and returns a string, the corresponding request string. This is to make sure that everywhere in the program the same strings are used. You need to add your constructor as a case and return the request string to it.

To now register your request, you need to add a new entry in the corresponding configuration. These are association lists that use the requests strings as keys and the description string as well as the operations you have implemented as values. Add a new entry with your request string as key and as value you use the constructor RegisteredField and give it the description string, the generator and the printer. After that, you are finished with implementing and adding your request to the program and it should now be possible to use like the others after compiling the changes.