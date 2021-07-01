## Design
The implementation of Scheml is centered around two design patterns - Interpreter
and the Command pattern.

### Interpreter Pattern
The Interpreter is actually two different interpreters as shown
here in the interface definition for the Expression interface (abbreviated here):
```java
public interface Expression {
  Expression evaluate(Environment<Expression> env, boolean inTailPosition)
          throws LispException;
  void unify(TypeRef typeRef, Environment<TypeRef> env)
          throws LispException;
}
```
The `evaluate` method is for actually computing a result from a Scheml
expression. Here is an example implementation of an if statement, where
it evaluates the test, and then depending on whether the test is true or
false, it evaluates the true or false option.
```java
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Expression testResult = test.evaluate(env, false);
        if (((BoolExpr)testResult).value) {
            return trueOption.evaluate(env, inTailPosition);
        } else {
            return falseOption.evaluate(env, inTailPosition);
        }
    }
```

The `unify` method is for type inference / type checking. It usually behaves
similarly to the `evaluate` method, but is only concerned with making sure that
the types are correct. In an if statement, for example, the test must be a
boolean expression, and both the true and false options must return the same
type. This is an abbreviated version of the `unify` method for an if statement:
```java

public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
    // Unify the test with boolean
    TypeRef testType = new TypeRef(BooleanType.TYPE);
    test.unify(testType, env);

    trueOption.unify(typeRef, env);
    
    // Unify the false option with the requested type ref, which also has the effect of unifying
    // it with the true option
    falseOption.unify(typeRef, env);
}
```
Although it is not shown in the above example, `unify` methods usually catch
UnifyException and augment the exception with the context of what was being
unified to help identify which statements cause problems.

### Command Pattern
It could be argued that what is being called the Command pattern here is not
exactly the Command pattern from the Gang-of-Four book. The Command pattern
typically encapsulates all the information it needs so that it can just be
executed at a later time. Scheml's usage of the Command pattern still needs
to be passed some context information to actually execute the command.

#### Special Forms
The first instance of a Command pattern is in how Scheml implements special
forms. In the Lisp family of languages, there are S-expressions that look
like typical function applications, but have different behavior with regard
to how the parts of the S-expressions are evaluated. For example, an if statement
in Scheml looks like this:
```
(if (> x 5)
    (printf "x is greater than 5\n")
    (printf "x is not greater than 5\n"))
```
The `(if)` here is a special form. It evaluates its first subexpression `(> x 5)`
but then only evaluates one of the other two subexpressions depending on the
result of the first subexpression.

The `if` special form, and the other special forms in Scheml, are all implemented
by classes that implement the Form interface:
```java
public interface Form {
  Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException;
}
```

The `FormExpander` class sets up a table of all the special form classes, like
this:
```java
    static {
        specialForms.put("define", new DefineForm());
        specialForms.put("if", new IfForm());
        specialForms.put("lambda", new LambdaForm());
        specialForms.put("list", new ListForm());
                   .
                   .
                   .
```
When an S-expression is loaded from a file or entered in the REPL, it is first
processed by the `FormExpander`, which looks at the first item in the list
and checks to see if it is a symbol corresponding to a special form, and if so,
it invokes the `expandForm` method in that special form implementation. To add
new special forms to Scheml, you just create a new class that implements
Form and add it to the `FormExpander`'s table.

#### Built-in Functions and Lambda Expressions
The other part of Scheml that uses something similar to the Command pattern, but
again requiring some bit of context, is the built-in function table. The
`BuiltinInitializer` class builds a table of built-in functions, all of which
are subclasses of `BuiltinFunctionExpr`. Many built-in functions are able
to take advantage of Java's lambda expressions, so that the function is given
as a lambda expression when creating the table:
```java
    static BuiltinFunctionExpr[] builtins = new BuiltinFunctionExpr[] {
        new BuiltinBinaryFunctionExpr<>("+", "int -> int -> int", (Long a, Long b) -> a+b),
        new BuiltinBinaryFunctionExpr<>("-", "int -> int -> int", (Long a, Long b) -> a-b),
        new BuiltinBinaryFunctionExpr<>("*", "int -> int -> int", (Long a, Long b) -> a*b),
        new BuiltinBinaryFunctionExpr<>("/", "int -> int -> int", (Long a, Long b) -> a/b),
                      .
                      .
                      .
```

The use of s-expressions as a syntactic representation is partially due to this
language being created as a final project for a course that covered some of
the functional features of Java as well as some well-known design patterns. It
uses the Interpreter pattern both for evaluating expressions, and for unifying
expressions in the type checker. Special forms use a mechanism like the
Command pattern where there is a registry of forms and each one is executed
the same way against an s-expression.

### Parser
Scheml uses S-expressions for several reasons:
* Parsing is simple and straightforward
* No need to worry about operator precedence
* Easy to extend with new special forms

The parser was written very early in the development process and did not
need modification to add new special forms.

### Type Inference
In addition to the `unify` method implementation in each expression, the
type inference system relies on the classes in the `type` package that represent
the various types within Scheml. An abstract type is represented by the
`AbstractType` and `AbstractTypeDecl` classes. An `AbstractType` object 
represents the general abstract type - that is, the type with its type
parameters, but not the type constructors. An `AbstractTypeDecl` represents
the whole declaration of the abstract type including each type constructor.

The `TypeRef` class is crucial for type inference because it contains the
logic to link classes together during unification, so that if there are several
references to unknown types that turn out to all refer to the same unknown
type, they are linked so that when the type is finally determined, each type
ref refers to the correct type. There are some tricky parts here because when
you copy a type that has several references that must refer to the same type,
there is some extra logic needed to preserve that linkage.

### Exhaustiveness / Usefulness Checking
Scheml supports a destructuring `match` statement that functions like
a switch or case statement in another language, and like some other strictly-typed
functional languages, it checks the match statements to see if they match
every possible value, although a non-exhaustive match statement currently
only generates a warning, and if the evaluation of a match ever fails to
match any of the patterns, the match statement will issue a fail exception.

The implementation of the exhaustiveness check uses the algorithm described
by Luc Maranget in "Warnings for Pattern Matching". Maranget's algorithm actually
determines whether or not a pattern statement is useful - that it matches something
that the patterns before it do not, and also gives a way to compute an example
pattern matched by the new pattern. In this way, Scheml is able to flag
statements as redundant, if the algorithm shows that they are not useful.
To perform exhaustiveness checking, we just check to see if "_" (the wildcard pattern)
is useful as the very last statement. If it is, then the previous statements
do not match all possible values.

### Tail Call Optimization
Most Java implementations do not provide tail call optimization, where a
function call in the tail position does not cause the stack to grow. Scheml
uses a trampoline mechanism to implement tail call optimization. Whenever
a function call is made in the tail position, rather than actually making the
call, the `ListExpr` class returns a `TailCallExpr` object. Since the function
call was made in the tail position of some other statement, the `TailCallExpr`
is returned from that other statement, and on down the call stack until
it gets to a function call that was not in the tail position. Once this happens,
the `TailCallExpr` is evaluated, performing the function call. The process
of returning the `TailCallExpr` down the call stack means that the tail call
is not using additional stack space.
