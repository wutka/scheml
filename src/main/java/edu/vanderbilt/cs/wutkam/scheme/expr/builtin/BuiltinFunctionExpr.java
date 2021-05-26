package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.FunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 11:21 AM
 */
public abstract class BuiltinFunctionExpr extends FunctionExpr {
    public String name;
    public BuiltinFunction builtinFunc;

    protected BuiltinFunctionExpr(String name, int arity) {
        super(arity, makeArgList(arity), null);
        this.name = name;
    }

    public BuiltinFunctionExpr(String name, int arity, BuiltinFunction builtinFunc) {
        super(arity, makeArgList(arity), null);
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
