package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 2:36 PM
 */
public class QuoteExpr implements Expression {
    Expression expr;

    public QuoteExpr(Expression expr) {
        this.expr = expr;
    }
    @Override
    public Expression evaluate(Environment<Expression> env) throws LispException {
        return expr;
    }
}
