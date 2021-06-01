package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.*;

import java.io.PushbackReader;
import java.io.StringReader;
import java.util.*;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/25/21
 * Time: 2:07 PM
 */
public class FunctionExpr implements Expression {
    public final int arity;
    public final List<Expression> partialArgs;
    public final List<Expression> targetExpressions;
    public final List<SymbolExpr> parameterList;

    public TypeRef[] paramTypes;
    public TypeRef returnType;

    public FunctionExpr(int arity, List<SymbolExpr> parameterList, List<Expression> targetExpressions) {
        this.arity = arity;
        this.parameterList = parameterList;
        this.targetExpressions = targetExpressions;
        this.partialArgs = new ArrayList<>();
    }

    public FunctionExpr(String signature) {
        FunctionType functionType = parseSignature(signature);
        this.arity = functionType.arity;
        this.parameterList = null;
        this.targetExpressions = null;
        this.partialArgs = new ArrayList<>();
        this.paramTypes = functionType.paramTypes;
        this.returnType = functionType.returnType;
    }
    public FunctionExpr(FunctionExpr partialFunc, List<Expression> partialArgs) {
        this.arity = partialFunc.arity;
        this.parameterList = partialFunc.parameterList;
        this.targetExpressions = partialFunc.targetExpressions;
        this.partialArgs = new ArrayList<>();
        this.partialArgs.addAll(partialFunc.partialArgs);
        this.partialArgs.addAll(partialArgs);
    }

    public void setType(TypeRef[] paramTypes, TypeRef returnType) {
        this.paramTypes = paramTypes;
        this.returnType = returnType;
    }

    @Override
    public String toString() {
        return "(function)";
    }

    @Override
    public Expression evaluate(Environment<Expression> env) throws LispException {
        return Expression.super.evaluate(env);
    }

    protected Expression apply(List<Expression> arguments, Environment<Expression> env)
        throws LispException {
        if (arguments.size() + partialArgs.size() > arity) {
            throw new LispException("Too many parameters passed to function "+this);
        }

        if (arguments.size() + partialArgs.size() == arity) {
            Environment<Expression> funcEnv = new Environment<>(env);
            for (int i=0; i < arity; i++) {
                if (i < partialArgs.size()) {
                    funcEnv.define(parameterList.get(i).value, partialArgs.get(i));
                } else {
                    funcEnv.define(parameterList.get(i).value, arguments.get(i-partialArgs.size()));
                }
            }
            Expression last = null;
            for (Expression target: targetExpressions) {
                last = target.evaluate(funcEnv);
            }
            return last;
        } else {
            return new FunctionExpr(this, arguments);
        }
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef[] paramTypeRefs = new TypeRef[arity];
        for (int i = 0; i < paramTypeRefs.length; i++) paramTypeRefs[i] = new TypeRef();

        for (int i = 0; i < partialArgs.size(); i++) {
            try {
                partialArgs.get(i).unify(paramTypeRefs[i], env);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Cannot unify function parameter " + paramTypeRefs[i].getType(), exc);
            }
        }

        Environment<TypeRef> funcEnv = new Environment<>(env);
        for (int i = 0; i < arity; i++) {
            String paramName = parameterList.get(i).value;
            funcEnv.define(paramName, paramTypeRefs[i]);
        }

        TypeRef returnType = new TypeRef();
        TypeRef lastExpr = new TypeRef();
        for (Expression expr: targetExpressions) {
            lastExpr = new TypeRef();
            try {
                expr.unify(lastExpr, funcEnv);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Can't unify function body", exc);
            }
        }
        try {
            returnType.unify(lastExpr);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify function return type", exc);
        }

        FunctionType thisType = new FunctionType(arity, paramTypeRefs, returnType);
        try {
            typeRef.unify(new TypeRef(thisType));
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify function with "+typeRef.getType(), exc);
        }
    }

    protected static FunctionType parseSignature(String signature) {
        try {
            PushbackReader pushback = new PushbackReader(new StringReader(signature));
            StringBuilder currSymbol = new StringBuilder();
            Stack<List<TypeRef>> typeStack = new Stack<>();
            Map<String,TypeRef> symbolNameMap = new HashMap<>();

            typeStack.push(new ArrayList<>());

            char ch;
            while ((ch = (char) pushback.read()) != (char) -1) {
                if (ch == '-') {
                    char ch2 = (char) pushback.read();
                    if (ch2 == (char) -1) {
                        throw new RuntimeException("Unexpected end of string after -");
                    }
                    if (ch2 != '>') {
                        throw new RuntimeException("Expected > after -");
                    }

                    String symbolName = currSymbol.toString().trim();
                    typeStack.peek().add(parseSymbolName(symbolName, symbolNameMap));
                    currSymbol = new StringBuilder();
                } else if (ch == '(') {
                    typeStack.push(new ArrayList<>());
                } else if (ch == ')') {
                    List<TypeRef> nestedList = typeStack.pop();
                    if (nestedList.size() == 1) {
                        throw new RuntimeException("Parenthesized expression should be a function or a construction, but had only one item");
                    }

                    TypeRef[] paramTypes = new TypeRef[nestedList.size()-1];
                    for (int i=0; i < paramTypes.length; i++) {
                        paramTypes[i] = nestedList.get(i);
                    }
                    TypeRef returnType = nestedList.get(nestedList.size()-1);
                    typeStack.peek().add(new TypeRef(new FunctionType(paramTypes.length, paramTypes, returnType)));
                } else {
                    currSymbol.append(ch);
                }
            }
            String symbolName = currSymbol.toString().trim();
            if (symbolName.length() > 0) {
                typeStack.peek().add(parseSymbolName(symbolName, symbolNameMap));
            }
            List<TypeRef> nestedList = typeStack.pop();
            if (nestedList.size() == 1) {
                throw new RuntimeException("Parenthesized expression should be a function, but had only one item");
            } else {
                TypeRef[] paramTypes = new TypeRef[nestedList.size()-1];
                for (int i=0; i < paramTypes.length; i++) {
                    paramTypes[i] = nestedList.get(i);
                }
                TypeRef returnType = nestedList.get(nestedList.size()-1);
                return new FunctionType(paramTypes.length, paramTypes, returnType);
            }

        } catch (Exception exc) {
            throw new RuntimeException("Error parsing function signature "+signature+": "+exc.getMessage(), exc);
        }
    }

    protected static TypeRef parseSymbolName(String symbolName, Map<String,TypeRef> symbolNameMap) {
        String[] parts = symbolName.split(" ");
        if (parts[0].equals("cons")) {
            if (parts.length < 2) {
                throw new RuntimeException("cons type needs a parameter");
            }
            TypeRef containedType = parseSymbolName(parts[1], symbolNameMap);
            return new TypeRef(new ConsType(containedType));
        } else if (parts[0].startsWith("'")) {
            TypeRef parametricType = symbolNameMap.get(parts[0]);
            if (parametricType == null) {
                parametricType = new TypeRef();
                symbolNameMap.put(parts[0], parametricType);
            }
            return parametricType;
        } else if (parts[0].equals("bool")) {
            return new TypeRef(BooleanType.TYPE);
        } else if (parts[0].equals("char")) {
            return new TypeRef(CharType.TYPE);
        } else if (parts[0].equals("double")) {
            return new TypeRef(DoubleType.TYPE);
        } else if (parts[0].equals("int")) {
            return new TypeRef(IntType.TYPE);
        } else if (parts[0].equals("string")) {
            return new TypeRef(StringType.TYPE);
        } else if (parts[0].equals("void")) {
            return new TypeRef(VoidType.TYPE);
        }
        throw new RuntimeException("Unknown type in type signature: "+parts[0]);
    }
}
