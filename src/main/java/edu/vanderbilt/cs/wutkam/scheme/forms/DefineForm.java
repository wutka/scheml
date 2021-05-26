package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.FunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 10:26 AM
 */
public class DefineForm implements Form {

    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (!isTopLevel) {
            throw new LispException("define is only allowed at the top level "+aList);
        }

        if (aList.elements.size() < 3) {
            throw new LispException("define must have two parameters");
        }
        if (aList.elements.get(1) instanceof ListExpr) {
            return defineFunction(aList);
        } else if (aList.elements.get(1) instanceof SymbolExpr) {
            return defineVariable(aList);
        } else {
            throw new LispException("Defined item must be a function or a symbol in "+aList.toString());
        }
    }

    protected Expression defineVariable(ListExpr aList) throws LispException {
        SymbolExpr sym = (SymbolExpr) aList.elements.get(1);
        Expression expr = aList.elements.get(2);
        if (expr instanceof ListExpr) {
            expr = FormExpander.expand((ListExpr) expr, false);
        }
        SchemeRuntime.getTopLevel().define(sym.value, expr);
        return expr;
    }

    protected Expression defineFunction(ListExpr aList) throws LispException {
        ListExpr functionHeader = (ListExpr) aList.elements.get(1);
        if (functionHeader.elements.size() == 0) {
            throw new LispException("Empty function header in "+functionHeader);
        }

        Expression functionName = functionHeader.elements.get(0);
        if (!(functionName instanceof SymbolExpr)) {
            throw new LispException("Function name must be a symbol in "+functionHeader);
        }

        List<Expression> functionBody = aList.elements.subList(2, aList.elements.size());

        FunctionExpr functionExpr = LambdaForm.createFunctionDefinition(
                new ListExpr(functionHeader.elements.subList(1, functionHeader.elements.size())),
                functionBody);
        SchemeRuntime.getTopLevel().define(((SymbolExpr)functionName).value, functionExpr);

        return functionExpr;
    }
}
