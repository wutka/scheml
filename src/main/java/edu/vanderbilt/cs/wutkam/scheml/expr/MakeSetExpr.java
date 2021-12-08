package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.SetType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;

import java.util.ArrayList;
import java.util.List;

public class MakeSetExpr implements Expression {
    List<Expression> initialValues;

    public MakeSetExpr(List<Expression> initialValues) throws LispException {
        this.initialValues = initialValues;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        List<Expression> evaluated = new ArrayList<>();
        for (Expression expr: initialValues) {
            Expression evaluatedElem = expr.evaluate(env, false);
            evaluated.add(evaluatedElem);
        }
        return new SetExpr(new ListExpr(evaluated));
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef initialElemType = new TypeRef();
        for (Expression expr: initialValues) {
            try {
                expr.unify(initialElemType, env);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Unable to unify set element types", exc);
            }
        }
        typeRef.unify(new TypeRef(new SetType(initialElemType)));
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("make-set"));
        for (Expression expr: initialValues) {
            scheml.add(expr.toScheml());
        }
        return new ListExpr(scheml);
    }
}
