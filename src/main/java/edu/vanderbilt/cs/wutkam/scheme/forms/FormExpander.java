package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.ApplyExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;

import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/24/21
 * Time: 4:21 PM
 */
public class FormExpander {
    public static Map<String,Form> specialForms = new HashMap<>();

    static {
        specialForms.put("if", new IfForm());
        specialForms.put("lambda", new LambdaForm());
        specialForms.put("define", new DefineForm());
        specialForms.put("quote", new QuoteForm());
        specialForms.put("let", new LetForm());
        specialForms.put("let*", new LetForm());
    }

    public static Expression expand(ListExpr aList, boolean isTopLevel) throws LispException
    {
        if (aList.elements.size() < 1) {
            return aList;
        }

        /*
        for (int i=0; i < aList.elements.size(); i++) {
            Expression elem = aList.elements.get(i);
            if (elem instanceof ListExpr) {
                aList.elements.set(i, expand((ListExpr) elem, false));
            }
        }
         */

        Expression firstElem = aList.elements.get(0);
        if (firstElem instanceof SymbolExpr) {
            SymbolExpr sym = (SymbolExpr) firstElem;
            Form expander = specialForms.get(sym.value);
            if (expander != null) {
                return expander.expandForm(aList, isTopLevel);
            }
        }
        return aList;
    }
}
