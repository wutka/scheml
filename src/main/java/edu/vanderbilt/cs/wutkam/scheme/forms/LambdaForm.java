package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.FunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 10:58 AM
 */
public class LambdaForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.elements.size() != 3) {
            throw new LispException("lambda should contain a parameter list and a body in "+aList);
        }
        Expression paramsExpr = aList.elements.get(1);
        if (!(paramsExpr instanceof ListExpr)) {
            throw new LispException("Lambda parameters must be a list in "+paramsExpr);
        }
        return createFunctionDefinition((ListExpr)paramsExpr, aList.elements.get(2));
    }

    protected static FunctionExpr createFunctionDefinition(ListExpr paramList, Expression functionBody)
        throws LispException {
        if (paramList.elements.size() == 0) {
            throw new LispException("Empty function header in "+paramList);
        }

        List<SymbolExpr> headerSyms = new ArrayList<>();
        for (Expression expr: paramList.elements) {
            if (!(expr instanceof SymbolExpr)) {
                throw new LispException("Function argument "+expr+" must be a symbol in "+paramList);
            }
            SymbolExpr sym = (SymbolExpr) expr;
            if (headerSyms.contains(sym)) {
                throw new LispException("Duplicate argument name "+sym.value+" in "+paramList);
            }
            headerSyms.add(sym);
        }
        if (functionBody instanceof ListExpr) {
            functionBody = FormExpander.expand((ListExpr) functionBody, false);
        }
        return new FunctionExpr(headerSyms.size(), headerSyms, functionBody);
    }
}
