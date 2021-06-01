package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.*;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.Type;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 11:21 AM
 */
public class BuiltinFunctionExpr extends FunctionExpr {
    public String name;
    public BuiltinFunction builtinFunc;

    protected BuiltinFunctionExpr(String name, String signature) {
        super(signature);
        this.name = name;
    }

    public BuiltinFunctionExpr(String name, String signature, BuiltinFunction builtinFunc) {
        super(signature);
        this.name = name;
        this.builtinFunc = builtinFunc;
    }

    protected Expression apply(List<Expression> arguments, Environment<Expression> env)
            throws LispException {
        if (arguments.size() + partialArgs.size() > arity) {
            throw new LispException("Too many parameters passed to function "+this);
        }

        if (arguments.size() + partialArgs.size() == arity) {
            Expression[] args = new Expression[arity];
            for (int i=0; i < arity; i++) {
                if (i < partialArgs.size()) {
                    args[i] = partialArgs.get(i).evaluate(env);
                } else {
                    args[i] = arguments.get(i-partialArgs.size()).evaluate(env);
                }
            }
            return executeBuiltin(args);
        } else {
            return new FunctionExpr(this, arguments);
        }
    }

    protected Expression executeBuiltin(Expression[] args) throws LispException {
        return builtinFunc.apply(args);
    }

    protected Expression fromJavaValue(Object obj) {
        if (obj instanceof Boolean) {
            return new BoolExpr((Boolean) obj);
        } else if (obj instanceof Character) {
            return new CharExpr((Character) obj);
        } else if (obj instanceof Double) {
            return new DoubleExpr((Double) obj);
        } else if (obj instanceof Integer) {
            return new IntExpr((Integer) obj);
        } else if (obj instanceof String) {
            return new StringExpr((String) obj);
        }
        return (Expression) obj;
    }

    static List<SymbolExpr> makeArgList(int n) {
        char argChar = 'a';
        ArrayList<SymbolExpr> symbolList = new ArrayList<>();
        for (int i=0; i < n; i++) {
            String argName;
            if (i < 52) {
                argName = ""+argChar;
            } else {
                argName = "arg"+i;
            }
            symbolList.add(new SymbolExpr("" + argName));
            if (argChar == 'z') {
                argChar = 'A';
            } else {
                argChar++;
            }
        }
        return symbolList;
    }
}
