package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.*;

/** Sorts a list according to a comparator function
 */
public class Sort extends BuiltinFunctionExpr {
    public Sort(String name) {
        super(name, "('a -> 'a -> int) -> cons 'a -> cons 'a");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        Applicable f = (Applicable) args[0];

        AbstractTypeExpr curr = (AbstractTypeExpr) args[1];
        List<Expression> sortList = new ArrayList<>();
        while (!curr.constructorName.equals("Nil")) {
            sortList.add(curr.values.get(0));
            curr = (AbstractTypeExpr) curr.values.get(1);
        }

        Collections.sort(sortList, (Expression a, Expression b) ->
                (int)((IntExpr)applyLoopNoExc(f, Arrays.asList(a, b))).value);

        AbstractTypeExpr destCurr = ConsTypeDecl.newNil();
        for (int i=sortList.size()-1; i >= 0; i--) {
            destCurr = ConsTypeDecl.newCons(sortList.get(i), destCurr);
        }
        return destCurr;
    }
}
