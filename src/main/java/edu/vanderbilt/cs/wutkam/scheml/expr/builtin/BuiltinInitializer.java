package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.array.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.ref.Ref;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.ref.SetRef;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.string.SplitWithLimit;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.VoidType;

/** An initializer for all the built-in functions.
 */
public class BuiltinInitializer {
    protected static BuiltinFunctionExpr[] builtins = new BuiltinFunctionExpr[] {
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

        new BuiltinUnaryFunctionExpr<>("->string", "'a -> string", Object::toString),
        new BuiltinUnaryFunctionExpr<>("id", "'a -> 'a", (Object a) -> a),
        new BuiltinUnaryFunctionExpr<>("string->int", "string -> int",
                (String a) -> Long.parseLong(a)),
        new BuiltinUnaryFunctionExpr<>("string->double", "string -> double", (String a) ->
                Double.parseDouble(a)),

        new ListToString("list->string"),
        new StringToList("string->list"),
        new Equals("equals?"),
        new TypeOf("type-of"),

        new Cons("cons"),
        new ToList("->list"),
        new Range("range"),
        new Head("head"),
        new Tail("tail"),
        new Empty("nil?"),
        new Empty("empty?"),
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
        new Length("length"),
        new Nth("nth"),
        new Map("map"),
        new Reverse("reverse"),
        new Append("append"),
        new Member("member?"),
        new Remove("remove"),
        new Take("take"),
        new Drop("drop"),
        new Fold("fold"),
        new All("all"),
        new Some("some"),
        new ReplaceNth("replace-nth"),
        new Filter("filter"),

        new Fst("fst"),
        new Snd("snd"),
        new ListConvertible("list-convertible?"),
        new ConvertSexprToList("convert-sexpr-list"),
        new IsSexprBool("sexpr-bool?"),
        new IsSexprChar("sexpr-char?"),
        new IsSexprDouble("sexpr-double?"),
        new IsSexprInt("sexpr-int?"),
        new IsSexprString("sexpr-string?"),
        new IsSexprSymbol("sexpr-symbol?"),
        new IsSexprList("sexpr-list?"),
        new SexprToBool("sexpr->bool"),
        new SexprToChar("sexpr->char"),
        new SexprToDouble("sexpr->double"),
        new SexprToInt("sexpr->int"),
        new SexprToString("sexpr->string"),
        new SexprToSymbol("sexpr->symbol"),
        new SexprToList("sexpr->list"),
        new Gensym("gensym"),
        new ParseFile("parse-file"),
        new ParseString("parse-string"),

        new Ref("!"),
        new SetRef("<-"),

        new ArrayFold("array-fold"),
        new ArrayLen("array-length"),
        new ArrayMap("array-map"),
        new ArrayRef("@"),
        new ArraySet("@<-"),
        new ArrayToList("array->list"),
        new ListToArray("list->array"),
        new MakeArrayWithDefault("make-array-with-default"),
        new MakeArrayWithFunction("make-array-with-function"),

        new DictFold("dict-fold"),
        new DictFoldKeys("dict-fold-keys"),
        new DictFoldValues("dict-fold-values"),
        new DictKeysToList("dict-keys"),
        new DictValuesToList("dict-values"),
        new DictLen("dict-len"),
        new DictLookup("dict-lookup"),
        new DictLookup("lookup"),
        new DictHasKey("dict-has-key?"),
        new DictHasKey("has-key?"),
        new DictMap("dict-map"),
        new DictToList("dict->list"),
        new ListToDict("list->dict"),

        new Split("split"),
        new SplitWithLimit("split-with-limit"),
    };

    public static void initializeBuiltins(Environment<Expression> exprEnv, Environment<TypeRef> typeEnv) {
        for (BuiltinFunctionExpr builtin: builtins) {
            exprEnv.define(builtin.name, builtin);
            exprEnv.define("void", new VoidExpr());
            typeEnv.define(builtin.name, new TypeRef(new FunctionType(builtin.paramTypes.length, builtin.paramTypes,
                    builtin.returnType)));
            typeEnv.define("void", new TypeRef(VoidType.TYPE));
        }
    }
}
