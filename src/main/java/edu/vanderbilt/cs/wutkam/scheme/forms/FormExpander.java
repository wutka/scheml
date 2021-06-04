package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
        specialForms.put("list", new ListForm());
        specialForms.put("let", new LetForm());
        specialForms.put("let*", new LetForm());
        specialForms.put("letrec", new LetForm());
        specialForms.put("match", new MatchForm());
        specialForms.put("progn", new PrognForm());
        specialForms.put("type", new TypeForm());
    }

    public static Expression expand(ListExpr aList, boolean isTopLevel) throws LispException
    {
        if (aList.size() < 1) {
            return aList;
        }

        /*
        for (int i=0; i < aList.size(); i++) {
            Expression elem = aList.getElement(i);
            if (elem instanceof ListExpr) {
                aList.elements.set(i, expand((ListExpr) elem, false));
            }
        }
         */

        Expression firstElem = aList.getElement(0);
        if (firstElem instanceof SymbolExpr) {
            SymbolExpr sym = (SymbolExpr) firstElem;
            Form expander = specialForms.get(sym.value);
            if (expander != null) {
                return expander.expandForm(aList, isTopLevel);
            }
        }

        List<Expression> expanded = new ArrayList<>();
        for (Expression expr: aList.elements) {
            if (expr instanceof ListExpr) {
                expanded.add(FormExpander.expand((ListExpr) expr, false));
            } else {
                expanded.add(expr);
            }
        }
        return new ListExpr(expanded);
    }
}
