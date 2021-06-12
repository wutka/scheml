package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.UnifyException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

/** Represents a list defined in an evaluated piece of code and should not be the result of any evaluation
 * of code. When this expression is evaluated, it is assumed to be a function application, which is consistent
 * with Scheme syntax.
 */
public class ListExpr implements Expression {
    private List<Expression> elements;

    public ListExpr() {
        elements = new ArrayList<>();
    }

    public ListExpr(List<Expression> elements) {
        this.elements = elements;
    }

    // Define some accessor methods so that elements isn't accessed directly
    public int size() {
        return this.elements.size();
    }

    public Expression getElement(int n) {
        return this.elements.get(n);
    }

    public List<Expression> elementsFrom(int n) {
        return this.elements.subList(n, this.elements.size());
    }

    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        if (elements.size() == 0) {
            throw new LispException("Cannot evaluate an empty list");
        }
        Expression targetExpression = elements.get(0);

        // Evaluate the target expression
        Expression maybeFunctionExpr = targetExpression.evaluate(env, false);

        // The target expression needs to be something that can be applied
        if (!(maybeFunctionExpr instanceof Applicable)) {
            throw new LispException("Expression "+maybeFunctionExpr.toString()+" is not a function");
        }

        // Evaluate the target function
        Applicable targetFunction = (Applicable) maybeFunctionExpr;

        // Create the parameter list for function application
        ArrayList<Expression> parameterList = new ArrayList<>();
        for (int i=1; i < elements.size(); i++) {
            parameterList.add(elements.get(i).evaluate(env, false));
        }

        // If this call is being made from the tail position (i.e. the return value of the function
        // is the return value of its parent expression, don't evaluate yet, but return a TailCallExpr
        // that will bubble back up the stack 
        if (inTailPosition) {
            return new TailCallExpr(targetFunction, parameterList, env.previous());
        } else {
            // Apply the function
            long startTime = System.nanoTime();
            Expression result = targetFunction.apply(parameterList, new Environment<>());

            // Since this call is not in the tail position, if we get a TailCallExpr, evaluate it, and keep
            // evaluating it while it returns a TailCallExpr
            while (result instanceof TailCallExpr) {
                result = result.evaluate(env, false);
            }

            long endTime = System.nanoTime();
            if (SchemlRuntime.getProfiler().enabled()) {
                if (targetFunction instanceof FunctionExpr) {
                    FunctionExpr funcExpr = (FunctionExpr) targetFunction;
                    if (funcExpr.name != null) {
                        SchemlRuntime.getProfiler().record(funcExpr.name, endTime - startTime);
                    }
                }
            }
            return result;
        }
    }

    @Override
    public void unify(TypeRef ref, Environment<TypeRef> env) throws LispException {
        if (elements.size() == 0) {
            throw new LispException("Cannot unify an empty list");
        }

        // Unify the target expression (the first expression)
        Expression targetExpression = elements.get(0);
        TypeRef targetType = new TypeRef();
        try {
            targetExpression.unify(targetType, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Unable to unify application target", exc);
        }

        FunctionType targetFunc;
        // If the target expression type is not a function, create a function expr with empty type refs
        // for the parameters and return type
        if (!(targetType.getType() instanceof FunctionType)) {
            TypeRef[] paramTypes = new TypeRef[elements.size()-1];
            for (int i=0; i < paramTypes.length; i++) paramTypes[i] = new TypeRef();
            TypeRef returnType = new TypeRef();
            targetFunc = new FunctionType(paramTypes.length, paramTypes, returnType);
        } else {
            targetFunc = (FunctionType) targetType.getType();
        }


        List<Expression> parameters = elements.subList(1, elements.size());
        if (parameters.size() > targetFunc.arity) {
            throw new UnifyException("Too many parameters in apply, expected "+targetFunc.arity+", got "+
                    parameters.size());
        }

        // Unify the provided function parameter types with those of the target function
        for (int i=0; i < parameters.size(); i++) {
            parameters.get(i).unify(targetFunc.paramTypes[i], env);
        }

        // If the target expression is a symbol, look up the symbol and then unify it with the targetFunc type
        // we are working with
        if (targetExpression instanceof SymbolExpr) {
            SymbolExpr sym = (SymbolExpr) targetExpression;
            TypeRef envRef = env.lookup(sym.value);
            if (envRef == null) {
                envRef = SchemlRuntime.getUnifyTopLevel().lookup(sym.value).copy(new HashMap<>());
            }
            envRef.unify(new TypeRef(targetFunc));
        }

        // If there aren't enough parameters, unify this as a partial function application where the partial
        // functions has as parameters only those parameters beyond what was provided here, and the same
        // return type
        if (parameters.size() < targetFunc.arity) {
            TypeRef[] newParamTypes = new TypeRef[targetFunc.arity - parameters.size()];
            for (int i=0; i < newParamTypes.length; i++) {
                newParamTypes[i] = targetFunc.paramTypes[i+parameters.size()];
            }
            FunctionType newTargetFunc = new FunctionType(newParamTypes.length, newParamTypes, targetFunc.returnType);

            // Since this is a partial application, the return type here is a partial function,
            // unify it with the requested type
            ref.unify(new TypeRef(newTargetFunc));
        } else {
            // Unify the return type with the requested type
            ref.unify(targetFunc.returnType);
        }
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        boolean first = true;
        for (Expression elem: elements) {
            if (!first) builder.append(' ');
            first = false;
            builder.append(elem.toString());
        }
        builder.append(')');
        return builder.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ListExpr listExpr = (ListExpr) o;
        return Objects.equals(elements, listExpr.elements);
    }

    @Override
    public int hashCode() {
        return Objects.hash(elements);
    }
}
