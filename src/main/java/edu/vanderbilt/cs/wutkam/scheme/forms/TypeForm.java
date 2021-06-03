package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;

import java.util.ArrayList;
import java.util.List;

public class TypeForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.elements.size() < 3) {
            throw new LispException("Type declaration must at least have a type name and a constructor name");
        }

        List<String> parametricTypes = new ArrayList<>();
        String typeName;
        List<Expression> typeConstructors;

        Expression typeNameExpr = aList.elements.get(1);
        if (typeNameExpr instanceof SymbolExpr) {
            typeName = ((SymbolExpr)typeNameExpr).value;
        } else {
            throw new LispException("Type name must be a symbol");
        }

        boolean finishedParametric = false;

        for (Expression expr: aList.elements.subList(2, aList.elements.size())) {
            if (expr instanceof SymbolExpr) {
                SymbolExpr sym = (SymbolExpr) expr;
                if (sym.value.startsWith("'")) {
                    if (finishedParametric) {
                        throw new LispException("All parametric types must immediately follow the type name");
                    }
                    parametricTypes.add(sym.value);
                } else {

                }
            }
        }
    }
}
