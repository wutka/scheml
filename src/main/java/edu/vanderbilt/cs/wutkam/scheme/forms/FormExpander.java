package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** Expands a form from a ListExpr into an expression. Each form then calls FormExpander on whatever
 * parts of it might need expansion (e.g. the body expressions of a function)
 */
public class FormExpander {
    public static Map<String,Form> specialForms = new HashMap<>();

    // Define the special forms used in this language
    static {
        specialForms.put("define", new DefineForm());
        specialForms.put("if", new IfForm());
        specialForms.put("lambda", new LambdaForm());
        specialForms.put("list", new ListForm());
        specialForms.put("let", new LetForm());
        specialForms.put("let*", new LetForm());
        specialForms.put("letrec", new LetForm());
        specialForms.put("match", new MatchForm());
        specialForms.put("printf", new PrintfForm());
        specialForms.put("progn", new PrognForm());
        specialForms.put("sprintf", new PrintfForm());
        specialForms.put("statements", new StatementsForm());
        specialForms.put("type", new TypeForm());
    }

    public static Expression expand(ListExpr aList, boolean isTopLevel) throws LispException
    {
        // Can't expand an empty form
        if (aList.size() < 1) {
            return aList;
        }

        // Look at the first element
        Expression firstElem = aList.getElement(0);

        // If it is symbol, see if there is an expander associated with it, and if so,
        // call the expander
        if (firstElem instanceof SymbolExpr) {
            SymbolExpr sym = (SymbolExpr) firstElem;
            Form expander = specialForms.get(sym.value);
            if (expander != null) {
                return expander.expandForm(aList, isTopLevel);
            }
        }

        // If this expression doesn't contain a special form, check to see if any expressions
        // it contains are special forms, and if so, expand them. This would be the case for
        // a function application like (print (if something "This" "That")) where print is not
        // a special form, but if is.
        List<Expression> expanded = new ArrayList<>();
        for (Expression expr: aList.elementsFrom(0)) {
            if (expr instanceof ListExpr) {
                expanded.add(FormExpander.expand((ListExpr) expr, false));
            } else {
                expanded.add(expr);
            }
        }
        return new ListExpr(expanded);
    }
}
