package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.FunctionExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashMap;
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

        if (aList.size() < 3) {
            throw new LispException("define must have two parameters");
        }
        if (aList.getElement(1) instanceof ListExpr) {
            return defineFunction(aList);
        } else if (aList.getElement(1) instanceof SymbolExpr) {
            return defineVariable(aList);
        } else {
            throw new LispException("Defined item must be a function or a symbol in "+aList.toString());
        }
    }

    protected Expression defineVariable(ListExpr aList) throws LispException {
        SymbolExpr sym = (SymbolExpr) aList.getElement(1);
        Expression expr = aList.getElement(2);
        if (expr instanceof ListExpr) {
            expr = FormExpander.expand((ListExpr) expr, false);
        }
        SchemeRuntime.getTopLevel().define(sym.value, expr);

        TypeRef typeRef = new TypeRef();
        expr.unify(typeRef, SchemeRuntime.getUnifyTopLevel());
        SchemeRuntime.getUnifyTopLevel().define(sym.value, typeRef);

        return expr;
    }

    protected Expression defineFunction(ListExpr aList) throws LispException {
        ListExpr functionHeader = (ListExpr) aList.getElement(1);
        if (functionHeader.size() == 0) {
            throw new LispException("Empty function header in "+functionHeader);
        }

        Expression functionNameExpr = functionHeader.elements.get(0);
        if (!(functionNameExpr instanceof SymbolExpr)) {
            throw new LispException("Function name must be a symbol in "+functionHeader);
        }

        List<Expression> functionBody = aList.elementsFrom(2);
        String functionName = ((SymbolExpr) functionNameExpr).value;

        FunctionExpr functionExpr = LambdaForm.createFunctionDefinitionWithName(functionName,
                new ListExpr(functionHeader.elementsFrom(1)),
                functionBody);
        SchemeRuntime.getTopLevel().define(functionName, functionExpr);

        Environment<TypeRef> unifyTopLevel = SchemeRuntime.getUnifyTopLevel();

        TypeRef[] paramTypes = new TypeRef[functionExpr.arity];
        for (int i=0; i < paramTypes.length; i++) paramTypes[i] = new TypeRef();

        FunctionType origFuncType = new FunctionType(functionExpr.arity, paramTypes, new TypeRef());
        unifyTopLevel.define(functionName, new TypeRef(origFuncType));

        TypeRef functionType = new TypeRef(origFuncType);
        functionExpr.unify(functionType, SchemeRuntime.getUnifyTopLevel());

        unifyTopLevel.define(functionName, functionType.copy(new HashMap<>()));

        return functionExpr;
    }
}
