package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 6/2/21
 * Time: 4:36 PM
 */
public class TailCallExpr implements Expression {
    public Applicable func;
    public List<Expression> arguments;
    public Environment<Expression> env;

    public TailCallExpr(Applicable func, List<Expression> arguments, Environment<Expression> env) {
        this.func = func;
        this.arguments = arguments;
        this.env = env;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        return func.apply(arguments, this.env);
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        // This expression should never appear during unification
    }
}
