package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.ConsType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/25/21
 * Time: 9:03 AM
 */
public class GeneratedConsExpr implements Expression {
    public final List<Expression> expressions;

    public GeneratedConsExpr(List<Expression> expressions) {
        this.expressions = expressions;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        ConsExpr curr = new ConsExpr();
        for (int i=expressions.size()-1; i >= 0; i--) {
            curr = new ConsExpr(expressions.get(i).evaluate(env, false), curr);
        }
        return curr;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef elementType = new TypeRef();
        for (Expression expr: expressions) {
            expr.unify(elementType, env);
        }

        typeRef.unify(new TypeRef(new ConsType(elementType)));
    }
}
