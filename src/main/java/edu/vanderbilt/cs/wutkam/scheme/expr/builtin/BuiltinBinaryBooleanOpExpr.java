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
public class BuiltinBinaryBooleanOpExpr extends BuiltinFunctionExpr
{
    BinaryBoolOperation boolOp;

    public BuiltinBinaryBooleanOpExpr(String name, BinaryBoolOperation boolOp)
    {
        super(name, 2, null);
        this.boolOp = boolOp;
        this.builtinFunc = (Expression[] args) -> executeBinaryOp(args);
    }

    public Expression executeBinaryOp(Expression[] args) throws LispException {
        if (args[0] instanceof BoolExpr) {
            boolean a = ((BoolExpr)args[0]).value;
            if (args[1] instanceof BoolExpr) {
                boolean b = ((BoolExpr) args[1]).value;
                return new BoolExpr(boolOp.apply(a, b));
            } else {
                throw new LispException("Second argument "+args[1]+" to "+this.name+" must be boolean");
            }
        } else {
            throw new LispException("First argument "+args[0]+" to "+this.name+" must be boolean");
        }
    }

}
