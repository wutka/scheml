package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolLiteralExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

import java.util.Arrays;

public class Gensym extends BuiltinFunctionExpr {
    private static long nextSym = 0;
    public Gensym(String name) {
        super(name, "void -> sexpr");
    }

    protected synchronized static long allocateSym() {
        return nextSym++;
    }

    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        long symNum = allocateSym();
        String symStr = "$$$GEN$$$"+Long.toString(symNum, 36);

        return new AbstractTypeExpr(SexprTypeDecl.sexprTypeName, "SexprSymbol",
                Arrays.asList(new SymbolExpr(symStr)));
    }
}
