package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.*;

import java.util.ArrayList;
import java.util.List;

/** Expands the form (lambda (arg-list) body) into a FunctionExpr expression
 */
public class LambdaForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 3) {
            throw new LispException("lambda should contain a parameter list and a body in "+aList);
        }
        Expression paramsExpr = aList.getElement(1);
        if (!(paramsExpr instanceof ListExpr)) {
            throw new LispException("Lambda parameters must be a list in "+paramsExpr);
        }
        return createFunctionDefinitionWithName(null, (ListExpr)paramsExpr, aList.elementsFrom(2));
    }

    protected static FunctionExpr createFunctionDefinitionWithName(String name, ListExpr paramList,
                List<Expression> functionBody) throws LispException {

        List<SymbolExpr> headerSyms = new ArrayList<>();

        // Check the parameter list and make sure that each parameter is a symbol and there are no duplicates
        for (Expression expr: paramList.elementsFrom(0)) {
            if (!(expr instanceof SymbolExpr)) {
                throw new LispException("Function argument "+expr+" must be a symbol in "+paramList);
            }
            SymbolExpr sym = (SymbolExpr) expr;
            if (headerSyms.contains(sym)) {
                throw new LispException("Duplicate argument name "+sym.value+" in "+paramList);
            }
            headerSyms.add(sym);
        }

        // Expand the function body if necessary
        List<Expression> body = new ArrayList<>();
        for (Expression expr: functionBody) {
            if (expr instanceof ListExpr) {
                expr = FormExpander.expand((ListExpr) expr, false);
            }
            body.add(expr);
        }

        // Create the FunctionExpr
        if (name == null) {
            return new ClosureExpr(name, headerSyms.size(), headerSyms, body);
        } else {
            return new FunctionExpr(name, headerSyms.size(), headerSyms, body);
        }
    }
}
