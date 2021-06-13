package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.List;

/** Represents a block of statements
 */
public class PrognExpr implements Expression {
    List<Expression> body;

    public PrognExpr(List<Expression> body) {
        this.body = body;
    }
    
    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Environment<Expression> prognEnv = new Environment<>(env);
        Expression last = null;
        for (int i=0; i < body.size(); i++) {
            Expression expr = body.get(i);
            // Only the last element is in tail position, and only if the progn itself is in tail position
            last = expr.evaluate(prognEnv, inTailPosition && (i == body.size() - 1));
        }
        // The return value is just the value of the last expression
        return last;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        Environment<TypeRef> prognEnv = new Environment<>(env);
        TypeRef returnType = null;
        for (Expression expr: body) {
            // Each statement's type is different, reset returnType each time to an empty type
            // so that after unifying each expression it will contain the value of the last expression
            returnType = new TypeRef();
            try {
                expr.unify(returnType, prognEnv);
            } catch (UnifyException exc)  {
                throw UnifyException.addCause("Error unifying expression in PROGN", exc);
            }
        }
        // Unify the return type with the requested type
        typeRef.unify(returnType);
    }
}
