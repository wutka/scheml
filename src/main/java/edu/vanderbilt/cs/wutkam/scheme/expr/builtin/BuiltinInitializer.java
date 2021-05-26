package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 1:19 PM
 */
public class BuiltinInitializer {
    static BuiltinFunctionExpr[] builtins = new BuiltinFunctionExpr[] {
        new BuiltinBinaryNumericOpExpr("+", (int a, int b) -> a+b, (double a, double b) -> a+b ),
        new BuiltinBinaryNumericOpExpr("-", (int a, int b) -> a-b, (double a, double b) -> a-b ),
        new BuiltinBinaryNumericOpExpr("*", (int a, int b) -> a*b, (double a, double b) -> a*b ),
        new BuiltinBinaryNumericOpExpr("/", (int a, int b) -> a/b, (double a, double b) -> a/b ),
        new BuiltinBinaryNumericOpExpr("div", (int a, int b) -> a/b, (double a, double b) -> a/b ),
        new BuiltinBinaryNumericOpExpr("mod", (int a, int b) -> a%b, (double a, double b) -> a%b ),
        new BuiltinBinaryNumericOpExpr("min", (int a, int b) -> Math.min(a,b), (double a, double b) -> Math.min(a,b) ),
        new BuiltinBinaryNumericOpExpr("max", (int a, int b) -> Math.max(a,b), (double a, double b) -> Math.max(a,b) ),
        new BuiltinBinaryNumericOpExpr("max", (int a, int b) -> Math.max(a,b), (double a, double b) -> Math.max(a,b) ),
        new BuiltinBinaryComparisonExpr("=", (double a, double b) -> a == b),
        new BuiltinBinaryComparisonExpr("!=", (double a, double b) -> a != b),
        new BuiltinBinaryComparisonExpr("<", (double a, double b) -> a < b),
        new BuiltinBinaryComparisonExpr(">", (double a, double b) -> a > b),
        new BuiltinBinaryComparisonExpr("<=", (double a, double b) -> a <= b),
        new BuiltinBinaryComparisonExpr(">=", (double a, double b) -> a >= b),
        new BuiltinUnaryNumericOpExpr("neg", (int a) -> -a, (double a) -> -a),
        new BuiltinBinaryBooleanOpExpr("and", (boolean a, boolean b) -> a && b),
        new BuiltinBinaryBooleanOpExpr("or", (boolean a, boolean b) -> a || b),
        new BuiltinBinaryBooleanOpExpr("xor", (boolean a, boolean b) -> a ^ b),
        new ListToString("list->string"),
        new StringToList("string->list"),
        new Equals("equals?"),
        new Cons("cons"),
        new Head("head"),
        new Tail("tail"),
        new Null("null?"),
        new Print("print"),
    };

    public static void initializeBuiltins(Environment<Expression> env) {
        for (BuiltinFunctionExpr builtin: builtins) {
            env.define(builtin.name, builtin);
        }
    }
}
