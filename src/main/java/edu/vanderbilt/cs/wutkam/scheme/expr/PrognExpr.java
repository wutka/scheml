package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 6:31 PM
 */
public class PrognExpr implements Expression {
    List<Expression> body;

    public PrognExpr(List<Expression> body) {
        this.body = body;
    }
    
    @Override
    public Expression evaluate(Environment<Expression> env) throws LispException {
        Expression last = null;
        for (Expression expr: body) {
            last = expr.evaluate(env);
        }
        return last;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef last = null;
        for (Expression expr: body) {
            TypeRef exprType = new TypeRef();
            try {
                expr.unify(exprType, env);
            } catch (UnifyException exc)  {
                throw UnifyException.addCause("Error unifying expression in PROGN", exc);
            }
            last = exprType;
        }
        typeRef.unify(last);
    }
}
