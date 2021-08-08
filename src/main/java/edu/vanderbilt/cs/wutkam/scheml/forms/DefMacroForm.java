package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;

import java.util.ArrayList;
import java.util.List;

public class DefMacroForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (!(aList.getElement(1) instanceof SymbolExpr)) {
            throw new LispException("Macro name "+aList.getElement(1)+" must be a symbol");
        }

        String name = ((SymbolExpr)aList.getElement(1)).value;
        Expression params = aList.getElement(2);
        if (!(params instanceof ListExpr)) {
            throw new LispException("Macro parameter list must be a list in macro "+name);
        }

        ListExpr paramList = (ListExpr) params;
        List<String> paramNames = new ArrayList<>();
        boolean lastIsRest = false;

        for (int i=0; i < paramList.size(); i++) {
            Expression param = paramList.getElement(i);
            if (!(param instanceof SymbolExpr)) {
                throw new LispException("Macro parameter name "+param+" must be a symbol in macro "+name);
            }
            String paramName = ((SymbolExpr)param).value;
            if (paramName.equals("&rest") || paramName.equals("&body")) {
                if (i != paramList.size()-2) {
                    throw new LispException("In macro "+name+",  "+paramName+" must be second-to-list in the param list");
                }
                lastIsRest = true;
            } else {
                paramNames.add(paramName);
            }
        }

        List<Expression> body = new ArrayList<>();

        for (Expression bodyExpr: aList.elementsFrom(3)) {
            if (bodyExpr instanceof ListExpr) {
                body.add(FormExpander.expand((ListExpr) bodyExpr, false));
            } else {
                body.add(bodyExpr);
            }
        }

        SchemlRuntime.getMacroTable().put(name, new Macro(name, paramNames, lastIsRest, body));

        return new VoidExpr();
    }
}
