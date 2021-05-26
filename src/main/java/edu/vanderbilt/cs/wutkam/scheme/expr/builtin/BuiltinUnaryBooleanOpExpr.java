package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.DoubleExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.IntExpr;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 12:52 PM
 */
public class BuiltinUnaryBooleanOpExpr extends BuiltinFunctionExpr
{
    protected final UnaryBooleanOperation boolOp;

    public BuiltinUnaryBooleanOpExpr(String name, UnaryBooleanOperation boolOp)
    {
        super(name, 2, null);
        this.boolOp = boolOp;
        this.builtinFunc = (Expression[] args) -> executeBinaryOp(args);
    }

    public Expression executeBinaryOp(Expression[] args) throws LispException {
        if (args[0] instanceof BoolExpr) {
            boolean a = ((BoolExpr)args[0]).value;
            return new BoolExpr(boolOp.apply(a));
        } else {
            throw new LispException("Argument "+args[0]+" to "+this.name+" must be boolean");
        }
    }

}
