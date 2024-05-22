### Package

Contains a directory called 'versions'.

### Version

Contains a json file and a directory called 'modules'.

### Module

Contains a json file, a directory called 'types', a directory called 'typeclasses' and a directory called 'operations'.

### Type

Contains a json file.

### Typeclass

Contains a json file.

### Operation

Contains a json file.

---

The respective json files contain the following information.

### Packages

* Versions ([String])

### Version

* Documentation (Doc)
* Categories ([String])
* Version (String)
* Modules ([String])

### Module

* Documentation (Doc)
* Source Code (Doc)
* Unsafe (Bool)
* Exports ([Export])
* Typeclasses ([Typeclass])
* Types ([Type])
* Functions ([Function])
* Operators ([Operator])
* Analysis Table (Analysis results for functions, types and typeclasses)

### Type

* Documentation (Doc)
* External/Constructors (Either External [Constructor])
* Definition (Type)

### Typeclass

* Documentation (Doc)
* Methods ([Signature])
* Definition (TypeClass)

### Operations

* Documentation (Doc)
* Source Code (Doc)
* Signature (Signature)
* Infix (Infix/InfixL/InfixR)
* Precedence (Int)
* Determinism/Nondeterminism (Det/NDet)
* Demandedness ([Int])
* Indeterminism (Bool)
* Solution Completeness (Bool)
* Termination (Bool)
* Totally Defined (Bool)
