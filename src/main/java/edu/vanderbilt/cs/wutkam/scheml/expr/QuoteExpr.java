package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

import java.util.ArrayList;
import java.util.Arrays;

public class QuoteExpr implements Expression {
    ListExpr expr;

    public QuoteExpr(ListExpr expr) {
        this.expr = expr;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        return SexprTypeDecl.fromList(expr, env);
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        typeRef.unify(new TypeRef(new AbstractType(ConsTypeDecl.consTypeName,
                Arrays.asList(new TypeRef(new AbstractType(SexprTypeDecl.sexprTypeName, new ArrayList<>()))))));
    }
}
