package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.PartialApplicationExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.forms.FormExpander;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

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

        Expression expr = SexprTypeDecl.toExpression((AbstractTypeExpr) arguments.get(0), true);
        if (expr instanceof ListExpr) {
            expr = FormExpander.expand((ListExpr) expr, false);
        }
        return SexprTypeDecl.fromExpression(expr.evaluate(env, false), env);
    }

    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        // Since this class overrides apply and doesn't actually call this method
        // it is safe to just return null
        return null;
    }
}
