package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.FunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;

import java.util.HashMap;
import java.util.List;

/** Defines a variable or a function with the form:
 *
 * (define var-name var-expression)  for a variable
 * or
 * (define (func-name arg1 arg2 ... argN) func-expression) for a function
 *
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
            throw new LispException("Defined item must be a function or a symbol in "+ aList);
        }
    }

    protected Expression defineVariable(ListExpr aList) throws LispException {
        SymbolExpr sym = (SymbolExpr) aList.getElement(1);
        Expression expr = aList.getElement(2);

        // If the variable value is an expression, make sure it gets expanded
        if (expr instanceof ListExpr) {
            expr = FormExpander.expand((ListExpr) expr, false);
        }
        expr = expr.evaluate(new Environment<>(), false);

        // Define the variable at the top level
        SchemlRuntime.getTopLevel().define(sym.value, expr);

        // Store the variable's computed type in the unify top level
        TypeRef typeRef = new TypeRef();
        try {
            expr.unify(typeRef, new Environment<>());
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Error defining symbol "+sym.value, exc);
        }
        SchemlRuntime.getUnifyTopLevel().define(sym.value, typeRef);

        return expr;
    }

    protected Expression defineFunction(ListExpr aList) throws LispException {
        ListExpr functionHeader = (ListExpr) aList.getElement(1);
        if (functionHeader.size() == 0) {
            throw new LispException("Empty function header in "+functionHeader);
        }

        Expression functionNameExpr = functionHeader.getElement(0);
        if (!(functionNameExpr instanceof SymbolExpr)) {
            throw new LispException("Function name must be a symbol in "+functionHeader);
        }

        List<Expression> functionBody = aList.elementsFrom(2);
        String functionName = ((SymbolExpr) functionNameExpr).value;

        // Since (define (func-name a b c) expr) is equivalent to
        // (define func-name (lambda (a b c) expr))
        // we just re-use the lambda form code to create the function definition
        FunctionExpr functionExpr = LambdaForm.createFunctionDefinitionWithName(functionName,
                new ListExpr(functionHeader.elementsFrom(1)),
                functionBody);

        // Store the function in the top level
        SchemlRuntime.getTopLevel().define(functionName, functionExpr);

        // Compute the function type and store that in the top level
        Environment<TypeRef> unifyTopLevel = SchemlRuntime.getUnifyTopLevel();

        TypeRef[] paramTypes = new TypeRef[functionExpr.arity];
        for (int i=0; i < paramTypes.length; i++) paramTypes[i] = new TypeRef();

        FunctionType origFuncType = new FunctionType(functionExpr.arity, paramTypes, new TypeRef());
        unifyTopLevel.define(functionName, new TypeRef(origFuncType));

        TypeRef functionType = new TypeRef(origFuncType);
        try {
            functionExpr.unify(functionType, new Environment<>());
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Error defining function " + functionName, exc);
        }

        unifyTopLevel.define(functionName, functionType.copy(new HashMap<>()));

        return functionExpr;
    }
}
