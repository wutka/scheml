package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.ArrayType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;

import java.util.ArrayList;
import java.util.List;

public class MakeArrayExpr implements Expression {
    List<Expression> initialValues;

    public MakeArrayExpr(List<Expression> initialValues) throws LispException {
        this.initialValues = initialValues;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        List<Expression> evaluated = new ArrayList<>();
        for (Expression expr: initialValues) {
            evaluated.add(expr.evaluate(env, false));
        }
        return new ArrayExpr(new ListExpr(evaluated));
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef initialValuesType = new TypeRef();
        for (Expression expr: initialValues) {
            try {
                expr.unify(initialValuesType, env);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Unable to unify array element types", exc);
            }
        }
        typeRef.unify(new TypeRef(new ArrayType(initialValuesType)));
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("make-array"));
        for (Expression expr: initialValues) {
            scheml.add(expr.toScheml());
        }
        return new ListExpr(scheml);
    }
}
