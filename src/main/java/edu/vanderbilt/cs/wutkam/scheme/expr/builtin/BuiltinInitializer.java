package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

/** An initializer for all the built-in functions.
 */
public class BuiltinInitializer {
    static BuiltinFunctionExpr[] builtins = new BuiltinFunctionExpr[] {
        new BuiltinBinaryFunctionExpr<>("+", "int -> int -> int", (Long a, Long b) -> a+b),
        new BuiltinBinaryFunctionExpr<>("-", "int -> int -> int", (Long a, Long b) -> a-b),
        new BuiltinBinaryFunctionExpr<>("*", "int -> int -> int", (Long a, Long b) -> a*b),
        new BuiltinBinaryFunctionExpr<>("/", "int -> int -> int", (Long a, Long b) -> a/b),
        new BuiltinBinaryFunctionExpr<>("%", "int -> int -> int", (Long a, Long b) -> a%b),
        new BuiltinBinaryFunctionExpr<>("div", "int -> int -> int", (Long a, Long b) -> a/b),
        new BuiltinBinaryFunctionExpr<>("mod", "int -> int -> int", (Long a, Long b) -> a%b),
        new BuiltinBinaryFunctionExpr<Long,Long,Long>("min", "int -> int -> int", Math::min),
        new BuiltinBinaryFunctionExpr<Long,Long,Long>("max", "int -> int -> int", Math::max),

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
        new BuiltinBinaryFunctionExpr<>("=", "int -> int -> bool", (Long a, Long b) -> a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("!=", "int -> int -> bool", (Long a, Long b) -> !a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("<", "int -> int -> bool", (Long a, Long b) -> a < b),
        new BuiltinBinaryFunctionExpr<>("<=", "int -> int -> bool", (Long a, Long b) -> a <= b),
        new BuiltinBinaryFunctionExpr<>(">", "int -> int -> bool", (Long a, Long b) -> a > b),
        new BuiltinBinaryFunctionExpr<>(">=", "int -> int -> bool", (Long a, Long b) -> a >= b),

        // As with the other numerical operations, there are separate comparison operators for doubles
        new BuiltinBinaryFunctionExpr<>("=.", "double -> double -> bool", (Double a, Double b) -> a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("!=.", "double -> double -> bool", (Double a, Double b) -> !a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("<.", "double -> double -> bool", (Double a, Double b) -> a < b),
        new BuiltinBinaryFunctionExpr<>("<=.", "double -> double -> bool", (Double a, Double b) -> a <= b),
        new BuiltinBinaryFunctionExpr<>(">.", "double -> double -> bool", (Double a, Double b) -> a > b),
        new BuiltinBinaryFunctionExpr<>(">=.", "double -> double -> bool", (Double a, Double b) -> a >= b),

        new BuiltinUnaryFunctionExpr<>("neg", "int -> int", (Long a) -> -a),
        new BuiltinUnaryFunctionExpr<>("neg.", "double -> double", (Double a) -> -a),

        new BuiltinUnaryFunctionExpr<>("int->double", "int->double", (Long a) -> (double) a),
        new BuiltinUnaryFunctionExpr<>("double->int", "double->int", (Double a) -> a.longValue()),

        new BuiltinUnaryFunctionExpr<>("int->char", "int->char", (Long a) -> (char) a.intValue()),
        new BuiltinUnaryFunctionExpr<>("char->int", "char->int", (Character a) -> (long) a),

        new BuiltinBinaryFunctionExpr<>("and", "bool -> bool -> bool", (Boolean a, Boolean b) -> a && b),
        new BuiltinBinaryFunctionExpr<>("or", "bool -> bool -> bool", (Boolean a, Boolean b) -> a || b),
        new BuiltinBinaryFunctionExpr<>("xor", "bool -> bool -> bool", (Boolean a, Boolean b) -> a ^ b),
        new BuiltinUnaryFunctionExpr<>("not", "bool -> bool", (Boolean a) -> !a),

        new BuiltinBinaryFunctionExpr<>("&", "int -> int -> int", (Long a, Long b) -> a & b),
        new BuiltinBinaryFunctionExpr<>("|", "int -> int -> int", (Long a, Long b) -> a | b),
        new BuiltinBinaryFunctionExpr<>("^", "int -> int -> int", (Long a, Long b) -> a ^ b),
        new BuiltinUnaryFunctionExpr<>("~", "int -> int", (Long a) -> ~a),
        new BuiltinBinaryFunctionExpr<>("<<", "int -> int -> int", (Long a, Long b) -> a << b),
        new BuiltinBinaryFunctionExpr<>(">>", "int -> int -> int", (Long a, Long b) -> a >> b),

        new BuiltinUnaryFunctionExpr<>("->string", "'a -> string", (Object a) -> a.toString()),
        new BuiltinUnaryFunctionExpr<>("id", "'a -> 'a", (Object a) -> a),

        new ListToString("list->string"),
        new StringToList("string->list"),
        new Equals("equals?"),
        new Cons("cons"),
        new ToList("->list"),
        new Range("range"),
        new Head("head"),
        new Tail("tail"),
        new Null("null?"),
        new Print("print"),
        new Load("load"),
        new Quit("quit"),
        new Input("input"),
        new ReadLines("read-lines"),
        new WriteLines("write-lines"),
        new Fail("fail"),
        new Split("split"),
        new Join("join"),
        new Profiling("profiling"),
    };

    public static void initializeBuiltins(Environment<Expression> exprEnv, Environment<TypeRef> typeEnv) {
        for (BuiltinFunctionExpr builtin: builtins) {
            exprEnv.define(builtin.name, builtin);
            typeEnv.define(builtin.name, new TypeRef(new FunctionType(builtin.paramTypes.length, builtin.paramTypes,
                    builtin.returnType)));
        }
    }
}
