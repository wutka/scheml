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
public class BuiltinBinaryComparisonExpr extends BuiltinFunctionExpr
{
    protected final BinaryCompareOperation compareOp;

    public BuiltinBinaryComparisonExpr(String name, BinaryCompareOperation compareOp)
    {
        super(name, 2, null);
        this.compareOp = compareOp;
        this.builtinFunc = (Expression[] args) -> executeBinaryOp(args);
    }

    public Expression executeBinaryOp(Expression[] args) throws LispException {
        if (args[0] instanceof IntExpr) {
            int a = ((IntExpr)args[0]).value;
            if (args[1] instanceof IntExpr) {
                int b = ((IntExpr) args[1]).value;
                return new BoolExpr(compareOp.apply(a, b));
            } else if (args[1] instanceof DoubleExpr) {
                double b = ((DoubleExpr) args[1]).value;
                return new BoolExpr(compareOp.apply(a, b));
            } else {
                throw new LispException("Second argument "+args[1]+" to "+this.name+" must be numeric");
            }
        } else if (args[0] instanceof DoubleExpr) {
            double a = ((DoubleExpr)args[0]).value;
            if (args[1] instanceof IntExpr) {
                int b = ((IntExpr) args[1]).value;
                return new BoolExpr(compareOp.apply(a, b));
            } else if (args[1] instanceof DoubleExpr) {
                double b = ((DoubleExpr) args[1]).value;
                return new BoolExpr(compareOp.apply(a, b));
            } else {
                throw new LispException("Second argument "+args[1]+" to "+this.name+" must be numeric");
            }
        } else {
            throw new LispException("First argument "+args[0]+" to "+this.name+" must be numeric");
        }
    }
}
