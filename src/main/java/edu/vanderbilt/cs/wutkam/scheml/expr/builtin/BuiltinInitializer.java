package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.expr.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.array.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.ref.Ref;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.ref.SetRef;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.set.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.string.SplitWithLimit;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.VoidType;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

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

        new BuiltinBinaryFunctionExpr<>("+b", "bignum -> bignum -> bignum", BigInteger::add),
        new BuiltinBinaryFunctionExpr<>("-b", "bignum -> bignum -> bignum", BigInteger::subtract),
        new BuiltinBinaryFunctionExpr<>("*b", "bignum -> bignum -> bignum", BigInteger::multiply),
        new BuiltinBinaryFunctionExpr<>("/b", "bignum -> bignum -> bignum", BigInteger::divide),
        new BuiltinBinaryFunctionExpr<>("%b", "bignum -> bignum -> bignum", BigInteger::mod),
        new BuiltinBinaryFunctionExpr<>("divb", "bignum -> bignum -> bignum", BigInteger::divide),
        new BuiltinBinaryFunctionExpr<>("modb", "bignum -> bignum -> bignum", BigInteger::mod),
        new BuiltinBinaryFunctionExpr<>("minb", "bignum -> bignum -> bignum", BigInteger::min),
        new BuiltinBinaryFunctionExpr<>("maxb", "bignum -> bignum -> bignum", BigInteger::max),

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

        new BuiltinBinaryFunctionExpr<>("=b", "bignum -> bignum -> bignum", BigInteger::equals),
        new BuiltinBinaryFunctionExpr<>("!=b", "bignum -> bignum -> bignum", (BigInteger a, BigInteger b) -> !a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("<b", "bignum -> bignum -> bignum", (BigInteger a, BigInteger b) -> a.compareTo(b) < 0),
        new BuiltinBinaryFunctionExpr<>("<=b", "bignum -> bignum -> bignum", (BigInteger a, BigInteger b) -> a.compareTo(b) <= 0),
        new BuiltinBinaryFunctionExpr<>(">b", "bignum -> bignum -> bignum", (BigInteger a, BigInteger b) -> a.compareTo(b) > 0),
        new BuiltinBinaryFunctionExpr<>(">=b", "bignum -> bignum -> bignum", (BigInteger a, BigInteger b) -> a.compareTo(b) >= 0),

        new BuiltinBinaryFunctionExpr<>("char=", "char -> char -> bool", (Character a, Character b) -> a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("char!=", "char -> char -> bool", (Character a, Character b) -> !a.equals(b)),
        new BuiltinBinaryFunctionExpr<>("char<", "char -> char -> bool", (Character a, Character b) -> a < b),
        new BuiltinBinaryFunctionExpr<>("char<=", "char -> char -> bool", (Character a, Character b) -> a <= b),
        new BuiltinBinaryFunctionExpr<>("char>", "char -> char -> bool", (Character a, Character b) -> a > b),
        new BuiltinBinaryFunctionExpr<>("char>=", "char -> char -> bool", (Character a, Character b) -> a >= b),

        new BuiltinUnaryFunctionExpr<>("neg", "int -> int", (Long a) -> -a),
        new BuiltinUnaryFunctionExpr<>("neg.", "double -> double", (Double a) -> -a),
        new BuiltinUnaryFunctionExpr<>("negb", "bignum -> bignum", BigInteger::negate),
        new BuiltinUnaryFunctionExpr<>("abs", "int -> int", (Long a) -> Math.abs(a)),
        new BuiltinUnaryFunctionExpr<>("abs.", "double -> double", (Double a) -> Math.abs(a)),
        new BuiltinUnaryFunctionExpr<>("absb", "bignum -> bignum", (BigInteger a) -> a.abs()),


            new BuiltinUnaryFunctionExpr<>("int->double", "int->double", (Long a) -> (double) a),
        new BuiltinUnaryFunctionExpr<>("double->int", "double->int", (Double a) -> a.longValue()),
        new BuiltinUnaryFunctionExpr<>("int->bignum", "int->bignum", (Long a) -> new BigInteger(a.toString())),
        new BuiltinUnaryFunctionExpr<>("bignum->int", "bignum->int", BigInteger::longValue),
        new BuiltinUnaryFunctionExpr<>("string->bignum", "string->bignum", (String a) -> new BigInteger(a)),
        new BuiltinUnaryFunctionExpr<>("bignum->string", "bignum->string", (BigInteger a) -> a.toString()),
        new BuiltinUnaryFunctionExpr<>("hexstring->bignum", "string->bignum", (String a) -> new BigInteger(a, 16)),
        new BuiltinUnaryFunctionExpr<>("bignum->hexstring", "bignum->string", (BigInteger a) -> a.toString(16)),

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
        new BuiltinUnaryFunctionExpr<String,Expression>("string->option-int", "string -> option int",
                (String a) -> { try { return new AbstractTypeExpr("option", "Just", Arrays.asList(new IntExpr(Long.parseLong(a)))); }
                                catch (Exception exc) { return new AbstractTypeExpr("option", "Nothing", new ArrayList<>()); }}),
        new BuiltinUnaryFunctionExpr<>("string->double", "string -> double", (String a) ->
                Double.parseDouble(a)),
        new BuiltinUnaryFunctionExpr<String,Expression>("string->option-double", "string -> option double",
                (String a) -> { try { return new AbstractTypeExpr("option", "Just", Arrays.asList(new DoubleExpr(Double.parseDouble(a)))); }
                catch (Exception exc) { return new AbstractTypeExpr("option", "Nothing", new ArrayList<>()); }}),
        new BuiltinUnaryFunctionExpr<>("string-length", "string -> int", String::length),
        new BuiltinUnaryFunctionExpr<>("string-toLowerCase", "string -> string", (String a) -> a.toLowerCase()),
        new BuiltinUnaryFunctionExpr<>("string-toUpperCase", "string -> string", (String a) -> a.toUpperCase()),
        new BuiltinBinaryFunctionExpr<>("string-at", "string -> int -> char", (String s, Long pos) -> s.charAt(pos.intValue())),
        new BuiltinBinaryFunctionExpr<>("string-index", "string -> char -> int", (String s, Character ch) -> s.indexOf(ch)),
        new BuiltinUnaryFunctionExpr<>("string-length", "string -> int", String::length),
        new BuiltinUnaryFunctionExpr<>("string-trim", "string -> string", String::trim),
        new BuiltinBinaryFunctionExpr<>("string-append", "string -> string -> string", (String a, String b) -> a + b),
        new BuiltinTernaryFunctionExpr<>("string-replace", "string -> string -> string -> string", String::replaceAll),

        new BuiltinUnaryFunctionExpr<>("char-lowercase?", "char -> bool", (Character ch) -> Character.isLowerCase(ch)),
        new BuiltinUnaryFunctionExpr<>("char-uppercase?", "char -> bool", (Character ch) -> Character.isUpperCase(ch)),
        new BuiltinUnaryFunctionExpr<>("char->lowercase", "char -> char", (Character ch) -> Character.toLowerCase(ch)),
        new BuiltinUnaryFunctionExpr<>("char->uppercase", "char -> char", (Character ch) -> Character.toUpperCase(ch)),

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
        new ParallelMap("pmap"),
        new MapOptional("map-optional"),
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
        new Sort("sort"),

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
        new DictLookupWithDefault("dict-lookup-with-default"),
        new DictLookup("lookup"),
        new DictHasKey("dict-has-key?"),
        new DictHasKey("has-key?"),
        new DictMap("dict-map"),
        new DictPut("dict-put"),
        new DictToList("dict->list"),
        new ListToDict("list->dict"),

        new ListToSet("list->set"),
        new SetContains("set-contains?"),
        new SetDifference("set-difference"),
        new SetIntersection("set-intersection"),
        new SetLen("set-len"),
        new SetPut("set-put"),
        new SetRemove("set-remove"),
        new SetToList("set->list"),
        new SetUnion("set-union"),

        new Split("split"),
        new SplitWithLimit("split-with-limit"),
        new RegexGroups("regex-groups"),
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
