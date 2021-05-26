package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.DoubleExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.IntExpr;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 12:52 PM
 */
public class BuiltinUnaryNumericOpExpr extends BuiltinFunctionExpr
{
    protected final UnaryIntOperation intOp;
    protected final UnaryDoubleOperation doubleOp;

    public BuiltinUnaryNumericOpExpr(String name, UnaryIntOperation intOp, UnaryDoubleOperation doubleOp)
    {
        super(name, 2, null);
        this.intOp = intOp;
        this.doubleOp = doubleOp;
        this.builtinFunc = (Expression[] args) -> executeBinaryOp(args);
    }

    public Expression executeBinaryOp(Expression[] args) throws LispException {
        if (args[0] instanceof IntExpr) {
            int a = ((IntExpr)args[0]).value;
            return new IntExpr(intOp.apply(a));
        } else if (args[0] instanceof DoubleExpr) {
            double a = ((DoubleExpr)args[0]).value;
            return new DoubleExpr(doubleOp.apply(a));
        } else {
            throw new LispException("Argument "+args[0]+" to "+this.name+" must be numeric");
        }
    }

}
