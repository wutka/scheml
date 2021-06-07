package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

/** An initializer for all the built-in functions.
 */
public class BuiltinInitializer {
    static BuiltinFunctionExpr[] builtins = new BuiltinFunctionExpr[] {
        new BuiltinBinaryFunctionExpr<>("+", "int -> int -> int", (Integer a, Integer b) -> a+b),
        new BuiltinBinaryFunctionExpr<>("-", "int -> int -> int", (Integer a, Integer b) -> a-b),
        new BuiltinBinaryFunctionExpr<>("*", "int -> int -> int", (Integer a, Integer b) -> a*b),
        new BuiltinBinaryFunctionExpr<>("/", "int -> int -> int", (Integer a, Integer b) -> a/b),
        new BuiltinBinaryFunctionExpr<>("%", "int -> int -> int", (Integer a, Integer b) -> a%b),
        new BuiltinBinaryFunctionExpr<>("div", "int -> int -> int", (Integer a, Integer b) -> a/b),
        new BuiltinBinaryFunctionExpr<>("mod", "int -> int -> int", (Integer a, Integer b) -> a%b),
        new BuiltinBinaryFunctionExpr<Integer,Integer,Integer>("min", "int -> int -> int", Math::min),
        new BuiltinBinaryFunctionExpr<Integer,Integer,Integer>("max", "int -> int -> int", Math::max),

        // Similar to Ocaml, floating point operations are separate from integer and have . after the
        // equivalent integer version
        new BuiltinBinaryFunctionExpr<>("+.", "double -> double -> double", (Double a, Double b) -> a+b),
        new BuiltinBinaryFunctionExpr<>("-.", "double -> double -> double", (Double a, Double b) -> a-b),
        new BuiltinBinaryFunctionExpr<>("*.", "double -> double -> double", (Double a, Double b) -> a*b),
        new BuiltinBinaryFunctionExpr<>("/.", "double -> double -> double", (Double a, Double b) -> a/b),
        new BuiltinBinaryFunctionExpr<>("%.", "double -> double -> double", (Double a, Double b) -> a%b),
        new BuiltinBinaryFunctionExpr<>("div.", "double -> double -> double", (Double a, Double b) -> a/b),
        new BuiltinBinaryFunctionExpr<>("mod.", "double -> double -> double", (Double a, Double b) -> a%b),
        new BuiltinBinaryFunctionExpr<Double,Double,Double>("min.", "double -> double -> double", Math::min),
        new BuiltinBinaryFunctionExpr<Double,Double,Double>("max.", "double -> double -> double", Math::max),

        // Only numeric comparisons are supported right now
        new BuiltinBinaryFunctionExpr<>("=", "int -> int -> bool", (Integer a, Integer b) -> a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("!=", "int -> int -> bool", (Integer a, Integer b) -> !a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("<", "int -> int -> bool", (Integer a, Integer b) -> a < b),
        new BuiltinBinaryFunctionExpr<>("<=", "int -> int -> bool", (Integer a, Integer b) -> a <= b),
        new BuiltinBinaryFunctionExpr<>(">", "int -> int -> bool", (Integer a, Integer b) -> a > b),
        new BuiltinBinaryFunctionExpr<>(">=", "int -> int -> bool", (Integer a, Integer b) -> a >= b),

        // As with the other numerical operations, there are separate comparison operators for doubles
        new BuiltinBinaryFunctionExpr<>("=.", "double -> double -> bool", (Double a, Double b) -> a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("!=.", "double -> double -> bool", (Double a, Double b) -> !a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("<.", "double -> double -> bool", (Double a, Double b) -> a < b),
        new BuiltinBinaryFunctionExpr<>("<=.", "double -> double -> bool", (Double a, Double b) -> a <= b),
        new BuiltinBinaryFunctionExpr<>(">.", "double -> double -> bool", (Double a, Double b) -> a > b),
        new BuiltinBinaryFunctionExpr<>(">=.", "double -> double -> bool", (Double a, Double b) -> a >= b),

        new BuiltinUnaryFunctionExpr<>("neg", "int -> int", (Integer a) -> -a),
        new BuiltinUnaryFunctionExpr<>("neg.", "double -> double", (Double a) -> -a),

        new BuiltinUnaryFunctionExpr<>("int->double", "int->double", (Integer a) -> (double) a),
        new BuiltinUnaryFunctionExpr<>("double->int", "double->int", (Double a) -> a.intValue()),

        new BuiltinBinaryFunctionExpr<>("and", "bool -> bool -> bool", (Boolean a, Boolean b) -> a && b),
        new BuiltinBinaryFunctionExpr<>("or", "bool -> bool -> bool", (Boolean a, Boolean b) -> a || b),
        new BuiltinBinaryFunctionExpr<>("xor", "bool -> bool -> bool", (Boolean a, Boolean b) -> a ^ b),

        new BuiltinUnaryFunctionExpr<>("not", "bool -> bool", (Boolean a) -> !a),

        new BuiltinUnaryFunctionExpr<>("->string", "'a -> string", (Object a) -> a.toString()),
        new BuiltinUnaryFunctionExpr<>("id", "'a -> 'a", (Object a) -> a),


        new ListToString("list->string"),
        new StringToList("string->list"),
        new Equals("equals?"),
        new Cons("cons"),
        new Range("range"),
        new Head("head"),
        new Tail("tail"),
        new Null("null?"),
        new Print("print"),
    };

    public static void initializeBuiltins(Environment<Expression> exprEnv, Environment<TypeRef> typeEnv) {
        for (BuiltinFunctionExpr builtin: builtins) {
            exprEnv.define(builtin.name, builtin);
            typeEnv.define(builtin.name, new TypeRef(new FunctionType(builtin.paramTypes.length, builtin.paramTypes,
                    builtin.returnType)));
        }
    }
}
