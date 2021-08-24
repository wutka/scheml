package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.UnifyException;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

import java.util.ArrayList;
import java.util.List;

public class Macro {
    protected String name;
    protected List<String> parameterNames;
    protected boolean lastIsRest;
    protected List<Expression> body;

    public Macro(String name, List<String> parameterNames, boolean lastIsRest,
                 List<Expression> body) {
        this.name = name;
        this.parameterNames = parameterNames;
        this.lastIsRest = lastIsRest;
        this.body = body;
    }

    public Expression expand(List<Expression> parameters) throws LispException {
        Environment<Expression> env = new Environment<>();
        Environment<TypeRef> unifyEnv = new Environment<>();
        for (int i=0; i < parameterNames.size(); i++) {
            String parameterName = parameterNames.get(i);
            if (lastIsRest && i == parameterNames.size()-1) {
                if (i < parameters.size()) {
                    env.define(parameterName, SexprTypeDecl.fromList(new ListExpr(parameters.subList(i, parameters.size())),
                            new Environment<>()));
                } else {
                    env.define(parameterName, SexprTypeDecl.fromList(new ListExpr(new ArrayList<>()),
                            new Environment<>()));
                }
            } else {
                if (i >= parameters.size()) {
                    throw new LispException("Not enough parameters to macro "+name);
                }
                env.define(parameterName, SexprTypeDecl.fromExpression(parameters.get(i), new Environment<>()));
            }
            unifyEnv.define(parameterName, new TypeRef(new AbstractType("sexpr", new ArrayList<>())));
        }

        Expression lastResult = null;
        for (int i=0; i < body.size(); i++) {
            Expression expr = body.get(i);
            try {
                expr.unify(new TypeRef(), unifyEnv);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Unable to unify expression in macro body", exc);
            }
            lastResult = expr.evaluate(env, i == body.size()-1);
        }

        if (lastResult instanceof AbstractTypeExpr) {
            AbstractTypeExpr abstractType = (AbstractTypeExpr) lastResult;
            if (abstractType.typeName.equals(SexprTypeDecl.sexprTypeName)) {
                return SexprTypeDecl.toExpression(abstractType, true);
            } else if (abstractType.typeName.equals(ConsTypeDecl.consTypeName)) {
               if (abstractType.constructorName.equals("Cons"))  {
                   Expression first = abstractType.values.get(0);
                   if (first instanceof AbstractTypeExpr) {
                       AbstractTypeExpr contained = (AbstractTypeExpr) first;
                       if (contained.typeName.equals(SexprTypeDecl.sexprTypeName)) {
                           return SexprTypeDecl.toList(abstractType, true);
                       }
                   }
               } else {
                   return new ListExpr();
               }
            }
        }

        return lastResult;
    }
}