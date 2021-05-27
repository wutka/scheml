package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

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
}
