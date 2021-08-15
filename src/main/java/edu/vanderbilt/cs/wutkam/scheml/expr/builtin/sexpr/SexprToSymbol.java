package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolLiteralExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.FailException;

public class SexprToSymbol extends BuiltinFunctionExpr {
    public SexprToSymbol(String name) { super(name, "sexpr -> symbol");
    }

    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = (AbstractTypeExpr) args[0];
        if (curr.constructorName.equals("SexprSymbol")) {
            Expression v = curr.values.get(0);
            if (v instanceof SymbolLiteralExpr) {
                return v;
            } else {
                return new SymbolLiteralExpr(((SymbolExpr)v).value);
            }
        } else {
            throw new FailException("S-expression is not a SexprSymbol");
        }
    }
}
