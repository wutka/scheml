package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.LetExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 3:30 PM
 */
public class LetForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.elements.size() != 3) {
            throw new LispException("let form requires declarations and a body");
        }

        boolean isLetStar = false;
        if (aList.elements.get(0) instanceof SymbolExpr) {
            SymbolExpr sym = (SymbolExpr) aList.elements.get(0);
            if (sym.value.equals("let*")) isLetStar = true;
        }
        if (!(aList.elements.get(1) instanceof ListExpr)) {
            throw new LispException("let form requires a list of declarations");
        }
        ListExpr decls = (ListExpr) aList.elements.get(1);
        List<LetExpr.Declaration> letDeclarations = new ArrayList<>();

        for (Expression declExpr: decls.elements) {
            if (!(declExpr instanceof ListExpr)) {
                throw new LispException("declaration "+declExpr+" in let should be a list with a name and expression");
            }
            ListExpr decl = (ListExpr) declExpr;
            if (decl.elements.size() != 2) {
                throw new LispException("declaration "+declExpr+" in let should be a list with a name and expression");
            }

            if (!(decl.elements.get(0) instanceof SymbolExpr)) {
                throw new LispException("name in declaration "+declExpr+" must be a symbol");
            }
            SymbolExpr sym = (SymbolExpr) decl.elements.get(0);
            Expression value = decl.elements.get(1);
            if (value instanceof ListExpr) {
                value = FormExpander.expand((ListExpr) value, false);
            }
            letDeclarations.add(new LetExpr.Declaration(sym.value, value));
        }

        List<Expression> body = new ArrayList<>();
        for (Expression expr: aList.elements.subList(2, aList.elements.size())) {
            if (expr instanceof ListExpr) {
                expr = FormExpander.expand((ListExpr) body, false);
            }
            body.add(expr);
        }
         
        return new LetExpr(letDeclarations, isLetStar, body);
    }
}
