# Scheml - Scheme Syntax, ML Semantics

Scheml behaves like a subset of ML, but with a Scheme-like syntax. Some of
the features that it has that make it different from vanilla Scheme:

* Type inference and strict typing
* Algebraic data types
* Destructuring pattern matching with usefulness and exhaustiveness checking
* Partial function application (and no variadic functions that aren't special forms)
* Tail-call optimization (this is actually expected of a Scheme or ML, but it's good
  to know it is available)

The use of s-expressions as a syntactic representation is partially due to this
language being created as a final project for a course that covered some of
the functional features of Java as well as some well-known design patterns. It
uses the Interpreter pattern both for evaluating expressions, and for unifying
expressions in the type checker. Special forms use a mechanism like the
Command pattern where there is a registry of forms and each one is executed
the same way against an s-expression.

## Building
You'll need a recent Java (I am using OpenJDK 14), and the Gradle build system.
To run the build and copy the resulting jar file to `scheml.jar` in the current
directory:
```shell
gradle copyJar
 ```

## Running
If you have done the build step above, then you can run Scheml one of two ways:

Using the `scheml` script in the root directory:
```shell
scheml
```

Or invoking Java directly:
```shell
java -jar scheml.jar
```

You should then see a prompt like this:
```shell
Scheml Repl
>
```

## Language Features

### Data types
Scheml supports the following data types:

#### bool
A boolean value whose representation is either `#t` or `#f`
Example:
```
#t
```

#### int
A 64-bit integer value (a Java long) currently only represented as
a decimal number (no hex/octal/binary representation yet)
Example:
```
1234567
```

#### char
A 16-bit character value (a Java char) using a Lisp-like #\a syntax. There
is currently no support for specifying characters with decimal, hex, or
binary, but there is an int->char function to create a char from an int value.
Example:
```
#\A   (the letter 'A')
```

### double
A 64-bit double-precision (Java double) value, currently only supporting
a format like _nnnnnnn.nnnnn_.
Example:
```
3.1415927
```

### string
A string of characters enclosed in double quotes. You can use \ to escape a "
and also use \n, \r, and \t as newline, carriage return, and tab.
Example:
```
"This is a string with a \" double quote and a newline\n"
```

### Algebraic Data Types
The `(type)` special form described below lets you define an algebraic data type.
You can create instances of these types with type constructors you define, and
you can use a `(match)` or in some cases `(let)` and `(statement)` to pull
individual fields from an algebraic data type.
```
(TreeNode "fred" EmptyNode EmptyNode)
```

### Functions
Since Scheml is a functional language, functions are first-class data items.
You can pass them around as parameters to other functions and apply them
when you want. 

### symbol (not really a type)
A symbol can be a function name or a variable name. Scheml is pretty lax in
what characters are allowed in a symbol. A symbol can't start with a digit,
and if it starts with -0 it is assumed to be a number, and if it starts
with #\ it is assumed to be a character constant, otherwise any of
these characters may appear in a symbol: `'*/+-!@#$%&_=:.<>?`.
Unlike Scheme, you can't manipulate symbols as data values, and they can't
appear as types in an expression.

## Special Forms
In the Lisp family of languages, special forms look like functions but their
semantics for evaluating expressions differs from that of function application.
Unlike Scheme or other Lisp-like languages, Scheml doesn't have any kind of
syntactic extension facility like macros. However, the source code is structured
in a way that makes it fairly easy to add new special forms, and the use of
S-expressions for the syntax means that you shouldn't have to modify the parser
to support a new special form.

### (if _test_ _true-expr_ _false-expr_)
The `(if)` special form works like it does in other Lisp-like languages. It
evaluates the _test_ expression, and if it is true, it then evaluates the
_true-expr_, and if it is false it evaluates the _false-expr_. Since Scheml is
strictly typed, both _true-expr_ and _false-expr_ must have the same type
and _test_ must be an express with a type of `bool`.
Example:
```
(if (> x 5)
  (print "x is too big\n")
  (print "x is okay\n"))
```

### (lambda (_args_) _body_)
The `(lambda)` form defines an anonymous function (although it can be
given a name via a `(let)` or `(statements)` special form). The items
in parens after the `lambda` should be zero or more symbols representing
the names of the parameters that should be passed to the function. After
that come the statements that make up the body of the function.
The result of the last statement is also the return value of the function.
Example:
```
(lambda (a b)
  (+ (* a a) (* b b)))
```