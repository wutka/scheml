package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.forms.FormExpander;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

import java.util.ArrayList;
import java.util.List;

public class Eval extends BuiltinFunctionExpr {
    public Eval(String name) {
        super(name, "sexpr -> sexpr");
    }

    @Override
    public Expression apply(List<Expression> arguments, Environment<Expression> env)
            throws LispException {
        if (arguments.size() > arity) {
            throw new LispException("Too many parameters passed to function "+this);
        }

        // If there aren't enough arguments supplied, return a partial function that will
        // invoke this function when all the arguments have been supplied
        if (arguments.size() < arity) {
            return new PartialApplicationExpr(this, arguments);
        }

        Expression expr = unliteralizeSymbols(SexprTypeDecl.toExpression((AbstractTypeExpr) arguments.get(0)));

        if (expr instanceof ListExpr) {
            expr = FormExpander.expand((ListExpr) expr, false);
        }
        return SexprTypeDecl.fromExpression(expr.evaluate(env, false).toScheml(), env);
    }

    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        // Since this class overrides apply and doesn't actually call this method
        // it is safe to just return null
        return null;
    }

    Expression unliteralizeSymbols(Expression expr) {
        if (expr instanceof SymbolLiteralExpr) {
            return new SymbolExpr(((SymbolLiteralExpr)expr).value);
        } else if (expr instanceof ListExpr) {
            List<Expression> exprs = new ArrayList<>();
            for (Expression e: ((ListExpr)expr).elementsFrom(0)) {
                exprs.add(unliteralizeSymbols(e));
            }
            return new ListExpr(exprs);
        } else {
            return expr;
        }
    }
}
