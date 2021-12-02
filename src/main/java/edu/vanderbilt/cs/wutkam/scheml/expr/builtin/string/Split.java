package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.string;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

public class Split extends BuiltinFunctionExpr {
    public Split(String name) {
        super(name, "string -> string -> cons string");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        String str = ((StringExpr)args[0]).value;
        String regex = ((StringExpr)args[1]).value;

        String[] split = str.split(regex);

        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        for (int i=split.length-1; i >= 0; i--) {
            curr = ConsTypeDecl.newCons(new StringExpr(split[i]), curr);
        }
        return curr;
    }
}
