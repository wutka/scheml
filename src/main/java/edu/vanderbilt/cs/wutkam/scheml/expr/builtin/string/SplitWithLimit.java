package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.string;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

public class SplitWithLimit extends BuiltinFunctionExpr {
    public SplitWithLimit(String name) {
        super(name, "string -> string -> int -> cons string");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        String str = ((StringExpr)args[0]).value;
        String regex = ((StringExpr)args[1]).value;
        long limit = ((IntExpr)args[2]).value;

        String[] split = str.split(regex, (int) limit);

        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        for (int i=split.length-1; i >= 0; i--) {
            curr = ConsTypeDecl.newCons(new StringExpr(split[i]), curr);
        }
        return curr;
    }
}
