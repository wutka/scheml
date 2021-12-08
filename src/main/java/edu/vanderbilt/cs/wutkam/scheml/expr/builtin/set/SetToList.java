package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.set;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.SetExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.Arrays;
import java.util.Map;

/** Converts a set to a list */
public class SetToList extends BuiltinFunctionExpr {
    public SetToList(String name) {
        super(name, "set 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        SetExpr set = (SetExpr) args[0];
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        for (Expression elem: set.set) {
             curr = ConsTypeDecl.newCons(elem, curr);
        }
        return curr;
    }
}
