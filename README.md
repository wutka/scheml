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

## A Quick Review For People Who Know Scheme
The basic forms described below should work the same way as Scheme. There
are a few differences, such as using `head` and `tail` for lists instead
of `car` and `cdr`. There are a couple of special forms that aren't in
Scheme like `:=`, `type`, and `match` and in addition to the usual types,
it also has abstract data types.

The big difference between Scheml and Scheme boils down to the fact that
in Scheml, lists cannot contain mixed data types. A list can't contain both
an int and a string. This restriction really inhibits the usefulness of
S-expressions in Scheml, since a Scheml program is not a valid expression
in Scheml, where in Scheme that is true, and allows you to create macros or
special syntactic constructs.

Scheml doesn't expose symbols to the programmer, and doesn't have a notion
of quoted values.

Because Scheml supports partial function application, there are situations
where you might use a `lambda` in Scheme that can be replaced with a
partial application. For example, to add 5 to every item in a list, instead of
the Scheme version `(map (lambda x) (+ 5 x) (list 1 2 3 4 5))` you can
just do `(map (+ 5) (list 1 2 3 4 5))`. On the other hand, because Scheml is
more strict about the types of functions, you can't make a function that takes
a variable number of arguments.

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
you can use a `(match)` or in some cases `(let)` and `(progn)` to pull
individual fields from an algebraic data type.
```
(TreeNode "fred" (EmptyNode) (EmptyNode))
```

### Lists
A list is actually an instance of an Algebraic Data Type with constructors named Nil
and Cons. Those definitions are built into the language and there is some support for
displaying a list in the form (1 2 3) instead of the internal representation which
would look like `(Cons 1 (Cons 2 (Cons 3 Nil)))`. In Lisp-like languages you can
create a quoted list like `'(1 2 3)` where the elements of the list are not evaluated
or you can use a function to create a list like `(list 1 2 3)`. In Scheml, because there
are no variadic functions (functions that can take a variable number of functions), you
can't define a `list` function that takes multiple parameters. However, there is a `list`
form that behaves like the list function in that `(list 1 2 3)` does create an expression
that is displayed as `(1 2 3)`. Likewise, `(list 1 2 3 4 5)` generates `(1 2 3 4 5)`.
Since `list` is implemented as a special form and not a function, it is able to take multiple
parameters, but this also means that since `list` isn't a function, you couldn't use it
as a parameter to a function. That is, you couldn't pass it to the `map` function like
(map list (some-func)).

There's one other important thing to realize about lists in Scheml:
**Lists cannot contain multiple types of variables**

You can't have a list of ints and strings like `(list 1 "foo" 3)`, you'll get the error:
```
Unification error:
Can't unify int with string
```

You can, on the other hand, have a list that contains items of the same Abstract Data Type
but that have multiple constructors, like `(list (Empty) (TreeNode 123 (Empty) (Empty)))`

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

## Comments
The semi-colon is the comment character, and it is in effect from where it starts
until the end of the current line (unless it occurs within a string).
Example
```
(define foo 123)  ; this is a comment
```

## Special Forms
In the Lisp family of languages, special forms look like functions but their
semantics for evaluating expressions differs from that of function application.
Unlike Scheme or other Lisp-like languages, Scheml doesn't have any kind of
syntactic extension facility like macros. However, the source code is structured
in a way that makes it fairly easy to add new special forms, and the use of
S-expressions for the syntax means that you shouldn't have to modify the parser
to support a new special form.

### (:= _var_ _expr_)
The `:=` operator, also referred to as `assign` allows you to store a variable
in the current evaluation environment. It is visible to any statements that come
after it in the environment, but not outside that environment. For example,
we can use `(progn)` to create a new evaluation environment (a function body
or a let body are also evaluation environments) and if we nest them, we can see
that changes to the local environment don't affect the parent:
```
(progn                                
  (:= foo "foo level 1")
  (printf "Foo is %s\n" foo)
  (progn
    (printf "Inside nested progn, foo is currently %s\n" foo)
    (:= foo "foo level 2")
    (printf "Inside nested progn, foo is now %s\n" foo))
  (printf "Outside of nested progn, foo is now %s\n" foo))
Foo is foo level 1
Inside nested progn, foo is currently foo level 1
Inside nested progn, foo is now foo level 2
Outside of nested progn, foo is now foo level 1
```

The `:=` uses the same semantics as a `let` binding (described below) so you can
also deconstruct abstract data types with it:

```
(:= (Pair first second) (function-that-returns-a-pair))
(printf "First part is %s, second part is %s\n" first second)
```

### (define _var_ _expr_)
The `define` form lets you define either a global value or a global function.
It is possible to redefine a value with another define statement. But you
can only do this from the top level of the Scheml REPL. That is, you can't
modify global variables from within a function.
Example:
```
(define foo "this is the foo string")
```

### (define (_func-name_ _args_) _body_)
This form of `define` lets you define a global function. The items in
parentheses are the name of the function and then a list of symbols that
are the parameters to the function. A function that takes no parameters would just
have `(_function-name_)` in this part. The body of the function is a sequence of
statements that may use any of the symbols in the function argument list including
the function name itself.
Example:
```
(define (multiply-by-7 a) (* a 7))
```

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

### (let/let*/letrec ((_binding_ _expr_) ...) _body_)
The three types of `(let)` allow you to bind values to variables within the
context of a body. You can destructure abstract types with variables, to extract
specific values.

The difference between `let`, `let*` and `letrec` is the scope in which the bound value
is visible. In a `let` binding the bound value is only visible in the body, it is not visible
in the let bindings that appear after it. For example:

```
(let ((foo 123)
      (bar (+ 5 foo)))   ;; <-- this is not allowed because foo is not visible until the body
  (printf "foo = %d, bar = %d\n" foo bar))
```

In a `let*` form, each binding is visible to the bindings that follow it. For example:
```
(let* ((foo 123)
      (bar (+ 5 foo)))   ;; <-- this is fine
  (printf "foo = %d, bar = %d\n" foo bar))
```

In a `letrec` form, the binding is visible within the expression being bound. This is mostly
useful for creating recursive lambda functions. For example, one commmon pattern in functional
languages is to make a tail-recursive version of a function that might take some extra parameters.
Here is an implementation of the factorial function that uses an accumulator for the products
so that it can be implemented tail-recursively. The tail-recursive function is implemented
as a lambda in a letrec expression, and then body of the letrec invokes the function. The outer
function then does not expose the accumulator to the user:
```
(define (fact n)
  (letrec 
      ((fact' (lambda (n acc)
                (if (< n 2) acc
                    (fact' (- n 1) (* n acc))))))
    (fact' n 1)))
```    

Note that you can use letrec to cause a stack overflow with a statement like this:
```
(letrec
  ((foo (list foo)))
  foo)
```
In the above case, you're declaring foo as a list of foo, which means it's a list of list of
foo, and so on.

As you can see in the above examples, you can bind a simple variable name to the result
of an expression. You can also unpack an abstract data type and assign its components
to variable names as well. If you don't care about a particular component, you can use `_`
as the variable name. For example, given the definition below of a Pair type constructor, you
can unpack both halves of the pair into separate variables:

```
(type pair ('a 'b) (Pair 'a 'b))
(define (func-that-returns-a-pair) (Pair "foo" 42))
(let (((Pair first-part second-part) (func-that-returns-a-pair)))
  (printf "The first part was %s and the second part %d\n"
      first-part second-part))
```
Notice that you need an extra pair of parens when deconstructing an abstract type because
the type constructor looks like a function. That is, in the previous let statements
where the binding for a simple variable was `(foo some-expression)`, the binding for an
abstract type is `((Pair foo bar) some-expression)`.

To maintain some level of type safety, the let binding currently only lets you bind
an abstract type if that type has only one constructor. If you need to work with an
abstract type that has multiple constructors, use a `(match)` form.

### (list item item item ... item)
As mentioned above, the `list` special form creates a list from any number of items.
It just creates an expression that evaluates to the abstract type
`(Cons item1 (Cons item2 (Cons item3 ... (Cons item-last Nil) ...)))`

### (match _expr_ (_pattern_ _expr) ...)
The `(match)` form allows you to deconstruct each case in an abstract type, but because
it also allows simple types in the match, it can act like a `case` or `switch` statement
in other languages. It evaluates the expression that appears just after `match` and then
looks for a pattern that matches that expression. If no patterns match, it raises an
exception.

Here is an example data type and a match statement that works with it:
```

(type tree-node ('a) 
  Empty
  (TreeNode 'a (tree-node 'a) (tree-node 'a)))

(match (function-that-returns-a-tree-node)
  (Empty (printf "The node is empty\n"))
  ((TreeNode data left right) (printf "The tree node contains data %s\n" data)))
```

Here is an example that matches simple data types:
```
(match (+ 3 4)
  (0 (printf "the result is 0\n"))
  (3 (printf "the result is 3\n"))
  (7 (printf "the result is 7\n"))
  (_ (printf "the result is something else\n"))
```

In the above example, the match will succeed against all possible values because the _
in the last pattern is a wildcard that matches anything. If you don't include that pattern,
you'll see a warning like this:
```
(match (+ 3 4)
  (0 (printf "the result is 0\n"))
  (3 (printf "the result is 3\n"))
  (7 (printf "the result is 7\n")))
the result is 7
Pattern match is not exhaustive, an unmatched pattern is 1
```
You will always get that warning when matching against ints, doubles, chars, and strings because
there are too many possibilities to match (yes, if you made 65,536 matchers, you _could_ avoid
the warning for chars).

Also, if a pattern match won't ever match because one of the previous patterns already
covers that case, you'll get a different warning:
```
(match "baz"
  ("foo" (printf "it was foo\n"))
  ("bar" (printf "it was bar\n"))
  ("baz" (printf "it was baz\n")))
it was baz
Pattern match is not exhaustive, an unmatched pattern is "quux"
```

Although you could use the Cons and Nil patterns to deconstruct a list, the `match` form
also allows you to use the more familiar list notation:
```
(match (list 1 2 3)
   ((1 2 4) (printf "it was (1 2 4)"))
   ((3 2 1) (printf "it was (3 2 1)"))
   ((1 2 3) (printf "it was (1 2 3)"))
   (_ (printf "it was something else")))
```
If you are matching against a list, just know that if you don't include _ as an option,
you will get a warning about the match being incomplete because lists can essentially be
any length, so no matter how long your match is, it will always fall one short.

Finally, when deconstructing an abstract type, the match can be recursive. Here's another
way to match the list (1 2 3)
```
(match (list 1 2 3)
  ((Cons 1 (Cons 2 (Cons 3 Nil))) (printf "It was (1 2 3)\n"))
  (_ (printf "It was something else\n")))
```

### (printf _format_ _args_)
You have probably been noticing all the `printf` calls in the various examples and might
have thought that `printf` is a function. But, as was the situation with the `list` form,
`printf` is a special form because it takes a variable number of arguments. 

Because Scheml is implemented in Java, the format string is the same as that specified in the
Java Formatter class here: https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html
with the exception that there is currently no support for the date/time conversions.

Since Scheml is strictly typed, it checks the types of the arguments to printf to make sure
they are correct, although the fact that the %s format in the Java Formatter can take any
object and just print the toString of it means that Scheml will allow you to pass any type
for %s. For a %d, however, the parameter must be an int, and for %f it must be a float.

### (progn _expr_ _expr_ _expr_ ... _expr_)
`progn` is probably a bit of an archaic name, but it just encapsulates a block of statements
so that the last statement is the return value of the block, similar to how `function` and
`let` bodies work.
Example:
```
(printf "Progn body returned %s\n"
   (progn (printf "running progn\n")
          (printf "still running progn\n")
          "foo"))
running progn
still running progn
Progn body returned foo

```

### (type [(_type-parameters_)] constructor constructor ...)
The `type` form lets you define abstract types. When you define an abstract type that
may contain varying types of object (if you haven't seen this before it's like template
parameters in Java & C++), and if you do that, you need to declare type variables for
the different abstract types. For example, a list is defined internally with this type:
```
(type cons ('a) Nil (Cons 'a (cons 'a)))
```

This means that the type named `cons` has one type parameter that for the purposes of this
declaration is named `'a`. If a constructor doesn't take any arguments, it
can just be specified by its name, as `Nil` is above, while those that do take
arguments must be parenthesized, as is `(Cons 'a (cons 'a))`. It is not a
typo that the second `cons` in this example is lowercase, it doesn't refer to
the `Cons` constructor but the `cons 'a` type, meaning that could be a `Nil` or
it could be another `Cons`.

Here's an example with multiple type parameters:
```
(type pair ('a 'b) (Pair 'a 'b)
```
This is a simple pair of items where each item can be any type. For example, the
expression `(Pair "foo" 42)` has the type `(pair string int)`.

Type constructors are functions, which means you can partially evaluate them.
This `(Pair 123)` is a partial function that has the type `'a -> pair int 'a`.
That is, it is a function that takes an argument of any type and returns a
pair of an int and that type.

## REPL features
The REPL is mostly just a way to enter expressions and see what they
evaluate to. But, there are two special commands:

### :r _filename_
The `:r` command reads in the named file and evaluates it.

### :t _expr_
Evaluates an expression and prints its type along with its value.

## Builtin Functions
Scheml has a number of built-in functions, mostly for dealing with strings and
numbers. Many of the common functions associated with lists are provided by
a separate lists.scm file.

### Arithmetic functions `+, -, *, /, div, mod, +., -. *., /., div. mod.`
Scheml doesn't do any kind of type conversion, and following the example of
Ocaml, it has separate operators for floating point operations. That is,
to add two ints, you use `+` but to add two doubles you use `+.`. The convention
is that the double version of an arithmetic or comparison function just has
a `.` after the int version of the same function. For example:
`(+ 3 4)` adds two integers, while `(+. 3.0 4.0)` adds two doubles.

The built-in functions can be partially applied, so `(+ 5)` is a partial
function that takes one argument and adds 5 to it. For example,
using the `map` function from `lists.scm`:
```
(map (+ 5) (list 1 2 3 4 5))
(6 7 8 9 10)
```

### Comparison functions `= != > >= < <= =. !=. >. >=. <. <=.`
As with the arithmetic functions, the comparison functions also have
separate int and double versions, although this could change in the future
because the return type is always bool.

### `equals?`
Unlike the `=` comparison function, the `equals?` function can test the
equality of any two objects. It just uses the Java equals method to compare
the object, but the simple expressions in Scheml, as well as the
abstract data types, all implement the Java equals method.

### `min max min. max.`
The `min` and `max` functions pick the minimum or maximum of two ints (or two
doubles for min. and max.).

### 'neg neg.'
The `neg` and `neg.` functions return the negative of an int or a double

### `int->double double->int`
The `int->double` and `double->int` functions provide conversions between int
and double.

### `and or xor not`
The `and`, `or`, `xor`, and `not` functions perform the boolean operations
their names indicate. Since `and` and `or` are functions whose arguments are
evaluated before they are called, they cannot do short-circuiting. If you
need a short-circuited and where the second argument isn't evaluated if the
first is false, do `(if first-test second-test #f)`. Similarly, for a
short-circuited `or` do `(if first-test #t second-test)`.

### `& | ^ ~ << >>`
The `&`, `|`, and `^` functions perform bitwise-and, -or, and -xor operations
respectively on ints. The `~` function does a bitwise invert on an int, and
`<<` and `>>` shift their first int argument left or right the number of bits
specified by their second int argument.

### `->string`
Converts any value to its string representation.

### `id`
The `id` function is the identity function that just returns its argument.

### `list->string` `string->list`
The `list->string` and `string->list` functions convert between a string
and a list of chars (not a list of any other type).

### `cons` `head` `tail` `->list` `empty?`
Like Scheme, Scheml uses `cons` to build a list, but it discards a piece
of Lisp history in not calling the `head` and `tail` functions `car` and
`cdr`. The `head` function returns the first item in a list, and the
`tail` function returns the rest of the list (everthing except for the
head). The `head` function will throw an exception if you try to take the
head of an empty list. The `->list` function turns any object into a one-object
list. Unlike the `(list)` form, you can map `->list` over a list to create
a list of lists.

The `empty?` function returns true if a list is empty.

### `range`
The `range` function takes two arguments, a beginning and end, and returns a
list of numbers from the first argument to the last argument **inclusive**. That
is, (range 1 5) returns the list (1 2 3 4 5).

### `print`
The `print` function prints a string to stdout.

### `input`
Takes no arguments, reads a line from stdin and returns it as a string
with the newline stripped off the end.

### `load`
The `load` function takes a string argument and loads and evaluates the
expressions in that file. It does the same thing as the `:r` command
in the REPL.

### `quit`
Terminates the REPL.

### `read-lines` `write-lines`
The `read-lines` function takes a string filename and reads all the lines
from the named file, returning them as a list. The lines will have all
the newlines stripped off the end.

The `write-lines` function takes a list of strings and a string filename,
and writes each string to the named file as a separate line, appending a
newline to each string.

### `fail`
Takes a string message and throws an exception to terminate the current
evaluation and return the REPL. The REPL will display the message.

### `split` `join`
The `split` function is a thin wrapper around the Java String split method,
in that it takes a string value, and also a string representation of a
regular expression, and splits the first string at the points where it
matches the regular expression. A simple `split` call would be:
```
(split "foo bar baz" " ")
("foo" "bar" "baz")
```

### `profiling`
Scheml includes a very basic profiler. You activate the profiler by calling
`(profiling #t)` and then do the operations you want to profile. When you
are done, call `(profiling #f)` and that will both stop the profiling and
also print out a report of the profiling, showing the number of calls of each
function, the total time each function too, and the average time each function
took.