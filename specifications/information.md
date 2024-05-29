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

* Version (String)
* Documentation (Doc)
* Categories ([String])
* Modules ([String])

### Module

* Documentation (Doc)
* Source Code (Doc)
* Unsafe (Bool)
* Exports ([Export])
* Typeclasses ([Typeclass])
* Types ([Type])
* Operations ([Operation])
* Analysis Table (Analysis results for functions, types and typeclasses)

### Type

* Documentation (Doc)
* Definition (Type)
* External/Constructors (Either External [Constructor])

### Typeclass

* Documentation (Doc)
* Definition (TypeClass)
* Methods ([Signature])

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
