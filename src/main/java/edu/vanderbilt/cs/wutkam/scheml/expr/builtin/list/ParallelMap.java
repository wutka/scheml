package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.list;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

/** Applies a function to every element of a list and returns a list of the results
 */
public class ParallelMap extends BuiltinFunctionExpr {
    public ParallelMap(String name) {
        super(name, "('a -> 'b) -> cons 'a -> cons 'b");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        // We don't don't check the types here, the type unification will take
        // care of that because of the function signature
        Applicable f = (Applicable) args[0];

        AbstractTypeExpr curr = (AbstractTypeExpr) args[1];
        List<Expression> listVals = new ArrayList<>();
        while (!curr.constructorName.equals("Nil")) {
            listVals.add(curr.values.get(0));
            curr = (AbstractTypeExpr) curr.values.get(1);
        }
        List<Expression> results = listVals.parallelStream().map((Expression expr) -> {
            List<Expression> argList = new ArrayList<>();
            argList.add(expr);
            try {
                return applyLoop(f, argList);
            } catch (Exception exc) {
                exc.printStackTrace();
                return null;
            }
        }).collect(Collectors.toList());

        AbstractTypeExpr destCurr = ConsTypeDecl.newNil();

        Collections.reverse(results);

        for (Expression expr: results) {
            destCurr = ConsTypeDecl.newCons(expr, destCurr);
        }
        return destCurr;
    }
}
