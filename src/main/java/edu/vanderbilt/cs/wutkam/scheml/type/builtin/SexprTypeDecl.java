package edu.vanderbilt.cs.wutkam.scheml.type.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.*;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.*;

import java.util.*;

/** A built-in AbstractTypeDecl for S-expressions (sexpr)
 */
public class SexprTypeDecl extends AbstractTypeDecl implements CustomToString {
    public static String sexprTypeName = "sexpr";
    static TypeRef parametricType = new TypeRef();

    public SexprTypeDecl() {
        super(sexprTypeName, Arrays.asList(new TypeRef()));
        this.addValueConstructors(createConstructorMap(this));
    }

    protected SexprTypeDecl(String typeName, List<TypeRef> parametricTypes) {
        super(typeName, parametricTypes);
    }

    protected SexprTypeDecl(String typeName, List<TypeRef> parametricTypes,
                            Map<String, ValueConstructorExpr> valueConstructors) {
        super(typeName, parametricTypes, valueConstructors);
    }

    @Override
    public AbstractTypeDecl copy() {
        // Create a linkage map so that all the named parametric types will be linked with their usage
        // in the various value constructors
        Map<String,TypeRef> linkageMap = new HashMap<>();
        List<TypeRef> newParametricTypes = new ArrayList<>();

        // Create a new list with copies of the parametric types
        for (TypeRef ref: parametricTypes) {
            newParametricTypes.add(ref.copy(linkageMap));
        }

        if (valueConstructors != null) {
            Map<String, ValueConstructorExpr> newValueConstructors = new HashMap<>();

            // Copy each value constructor
            for (String key : valueConstructors.keySet()) {
                ValueConstructorExpr valueConstructor = valueConstructors.get(key);

                // Create copies of the parametric types in the constructor (the linkage map will make them
                // refer to the same types as those from the abstract type itself
                List<TypeRef> parametricTypes = new ArrayList<>();
                for (TypeRef ref : valueConstructor.parametricTypes) {
                    parametricTypes.add(ref.copy(linkageMap));
                }

                // Create copies of the parameter types for the value constructor, again linking them
                // with the parametric types in the abstract type and the other constructors
                List<TypeRef> paramTypes = new ArrayList<>();
                for (int i = 0; i < valueConstructor.paramTypes.length; i++) {
                    paramTypes.add(valueConstructor.paramTypes[i].copy(linkageMap));
                }

                // Add this constructor to the new constructor map
                newValueConstructors.put(key, new ValueConstructorExpr(valueConstructor.typeName,
                        valueConstructor.name, parametricTypes, paramTypes));
            }
            // Return the new type decl
            return new SexprTypeDecl(typeName, newParametricTypes, newValueConstructors);
        } else {
            // If we don't have the constructors yet, just return a copy of the decl without them
            return new SexprTypeDecl(typeName, newParametricTypes);
        }
    }

    /** Renders this abstract type as a simple list */
    public String customToString(AbstractTypeExpr expr) {
        return expr.values.get(0).toString();
    }

    static ValueConstructorExpr[] constructors = new ValueConstructorExpr[] {
         new ValueConstructorExpr(sexprTypeName, "SexprBool", Arrays.asList(new TypeRef(BooleanType.TYPE))),
         new ValueConstructorExpr(sexprTypeName, "SexprInt", Arrays.asList(new TypeRef(IntType.TYPE))),
         new ValueConstructorExpr(sexprTypeName, "SexprChar", Arrays.asList(new TypeRef(CharType.TYPE))),
         new ValueConstructorExpr(sexprTypeName, "SexprDouble", Arrays.asList(new TypeRef(DoubleType.TYPE))),
         new ValueConstructorExpr(sexprTypeName, "SexprString", Arrays.asList(new TypeRef(StringType.TYPE))),
         new ValueConstructorExpr(sexprTypeName, "SexprSymbol", Arrays.asList(new TypeRef(SymbolType.TYPE))),
         new ValueConstructorExpr(sexprTypeName, "SexprUnquoteSymbol", Arrays.asList(new TypeRef(SymbolType.TYPE))),
        null,
    };

    protected static Map<String, ValueConstructorExpr> createConstructorMap(SexprTypeDecl decl) {

        ValueConstructorExpr listConstructor = new ValueConstructorExpr(sexprTypeName, "SexprList",
                Arrays.asList(new TypeRef(new AbstractType("cons", Arrays.asList(new TypeRef(new AbstractType(decl)))))));
        constructors[constructors.length-1] = listConstructor;


        Map<String, ValueConstructorExpr> sexprMap = new HashMap<>();
        for (ValueConstructorExpr valueCons: constructors) {
            sexprMap.put(valueCons.name, valueCons);
            SchemlRuntime.getTopLevel().define(valueCons.name, valueCons);
            SchemlRuntime.getUnifyTopLevel().define(valueCons.name,
                    new TypeRef(new FunctionType(valueCons.paramTypes.length, valueCons.paramTypes,
                            new TypeRef(new AbstractType(decl)))));
        }
        return sexprMap;
    }

    public static Expression toExpression(AbstractTypeExpr abstractExpr) throws LispException {
        if (abstractExpr.constructorName.equals("SexprBool") ||
            abstractExpr.constructorName.equals("SexprInt") ||
            abstractExpr.constructorName.equals("SexprChar") ||
            abstractExpr.constructorName.equals("SexprDouble") ||
            abstractExpr.constructorName.equals("SexprString") ||
            abstractExpr.constructorName.equals("SexprSymbol")) {
            return abstractExpr.values.get(0);
        } else if (abstractExpr.constructorName.equals("SexprList")) {
            return toList((AbstractTypeExpr)abstractExpr.values.get(0));
        } else {
            throw new LispException("Unable to convert abstract type "+abstractExpr.constructorName+" to an expression");
        }
    }

    public static ListExpr toList(AbstractTypeExpr abstractExpr) throws LispException {
        List<Expression> exprList = new ArrayList<>();
        while (abstractExpr.constructorName.equals("Cons")) {
            exprList.add(abstractExpr.values.get(0));
            abstractExpr = (AbstractTypeExpr) abstractExpr.values.get(1);
        }
        return new ListExpr(exprList);
    }

    public static AbstractTypeExpr fromExpression(Expression expr, Environment<Expression> env) throws LispException {
        if (expr instanceof BoolExpr) {
           return new AbstractTypeExpr(sexprTypeName, "SexprBool", Arrays.asList(expr));
        } else if (expr instanceof IntExpr) {
            return new AbstractTypeExpr(sexprTypeName, "SexprInt", Arrays.asList(expr));
        } else if (expr instanceof CharExpr) {
            return new AbstractTypeExpr(sexprTypeName, "SexprChar", Arrays.asList(expr));
        } else if (expr instanceof DoubleExpr) {
            return new AbstractTypeExpr(sexprTypeName, "SexprDouble", Arrays.asList(expr));
        } else if (expr instanceof StringExpr) {
            return new AbstractTypeExpr(sexprTypeName, "SexprString", Arrays.asList(expr));
        } else if (expr instanceof SymbolExpr) {
            String value = ((SymbolExpr)expr).value;
            return new AbstractTypeExpr(sexprTypeName, "SexprSymbol", Arrays.asList(new QuotedSymbol(value)));
        } else if (expr instanceof ListExpr) {
            ListExpr listExpr = (ListExpr) expr;
            if (listExpr.size() > 1) {
                Expression first = listExpr.getElement(0);
                if (first instanceof SymbolExpr) {
                    String symbol = ((SymbolExpr) first).value;
                    if (symbol.equals("quote")) {
                        return fromList((ListExpr) listExpr.getElement(1), env);
                    } else if (symbol.equals("unquote")) {
                        return new AbstractTypeExpr(sexprTypeName, "SexprUnquoteSymbol", Arrays.asList(
                                fromExpression(listExpr.getElement(1).evaluate(env, false), env)));
                    }
                }
            }
            return fromList(listExpr, env);
        }
        throw new LispException("Unable to convert expression "+expr+" to S-expression");
    }

    public static AbstractTypeExpr fromList(ListExpr listExpr, Environment<Expression> env) throws LispException {
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        for (int i=listExpr.size()-1; i >= 0; i--) {
            Expression expr = listExpr.getElement(i);
            if (expr instanceof ListExpr) {
                Expression first = ((ListExpr)expr).getElement(0);
                if (first instanceof SymbolExpr) {
                    String symbol = ((SymbolExpr)first).value;
                    if (symbol.equals("unquote-splice")) {
                        Expression unquoted = ((ListExpr)expr).getElement(1).evaluate(env, false);
                        if (unquoted instanceof AbstractTypeExpr) {
                            AbstractTypeExpr abstractType = (AbstractTypeExpr) unquoted;
                            if (abstractType.constructorName.equals("Nil")) {
                                continue;
                            } else if (abstractType.constructorName.equals("Cons")) {
                                Stack<Expression> spliceStack = new Stack<>();
                                while (abstractType.constructorName.equals("Cons")) {
                                    spliceStack.push(abstractType.values.get(0));
                                    abstractType = (AbstractTypeExpr) abstractType.values.get(1);
                                }
                                while (!spliceStack.isEmpty()) {
                                    curr = ConsTypeDecl.newCons(fromExpression(spliceStack.pop(), env), curr);
                                }
                                continue;
                            } else {
                                throw new LispException("Cannot splice "+abstractType.constructorName+
                                        " expression into list (use , instead of ,@ ?)");
                            }
                        } else {
                            throw new LispException("Cannot splice non-list into list (use , instead of ,@ ?)");
                        }
                    }
                }
            }
            curr = ConsTypeDecl.newCons(fromExpression(listExpr.getElement(i), env), curr);
        }
        return curr;
    }
}
