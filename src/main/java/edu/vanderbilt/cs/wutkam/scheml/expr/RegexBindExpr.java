package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.ExhaustivenessChecker;
import edu.vanderbilt.cs.wutkam.scheml.expr.match.Match;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Runs a regex against a string, binds group values to variables and
 * executes the body within the context of the variables, wrapping any
 * result value with an option.
 */
public class RegexBindExpr implements Expression {
    public final Expression targetExpression;
    public final String regex;
    public final Pattern regexPattern;
    public final List<RegexBindVariable> vars;
    public final List<Expression> body;

    public RegexBindExpr(Expression targetExpression, String regex, List<RegexBindVariable> vars,
                         List<Expression> body) {
        this.targetExpression = targetExpression;
        this.regex = regex;
        this.regexPattern = Pattern.compile(regex);
        this.vars = vars;
        this.body = body;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Expression targetResult = targetExpression.evaluate(env, false);
        String target = ((StringExpr)targetResult).value;
        Matcher matcher = regexPattern.matcher(target);
        if (!matcher.matches()) {
            return new AbstractTypeExpr("option", "Nothing", new ArrayList<>());
        }

        Environment<Expression> bindEnv = new Environment<>(env);
        for (int i=0; i < vars.size(); i++) {
            String groupValue = matcher.group(i+1);
            StringExpr groupValueExpr = new StringExpr(groupValue);
            RegexBindVariable var = vars.get(i);
            if (var.processingFunc != null) {
                Applicable f = (Applicable) var.processingFunc;
                List<Expression> args = Arrays.asList(groupValueExpr);
                Expression fResult = f.apply(args, env);
                bindEnv.define(var.name, fResult);
            } else {
                bindEnv.define(var.name, groupValueExpr);
            }
        }

        Expression result = null;

        for (int i=0; i < body.size(); i++) {
            Expression bodyExpr = body.get(i);
            result = bodyExpr.evaluate(bindEnv, false);
        }

        return new AbstractTypeExpr("option", "Just", Arrays.asList(result));
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef targetType = new TypeRef(StringType.TYPE);
        try {
            targetExpression.unify(targetType, env);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("regex-bind target must be a string", exc);
        }

        Environment<TypeRef> bindEnv = new Environment<>(env);
        for (RegexBindVariable v: vars) {
            if (v.processingFunc != null) {
                TypeRef paramType = new TypeRef(StringType.TYPE);
                TypeRef returnType = new TypeRef();
                FunctionType funcType = new FunctionType(1, new TypeRef[] { paramType }, returnType);
                TypeRef funcTypeRef = new TypeRef(funcType);
                try {
                    v.processingFunc.unify(funcTypeRef, env);
                } catch (UnifyException exc) {
                    throw UnifyException.addCause(
                            "regex-bind processing function must take a string and return an expression", exc);
                }
                env.define(v.name, funcType.returnType);
            } else {
                TypeRef varType = new TypeRef(StringType.TYPE);
                env.define(v.name, varType);
            }
        }

        TypeRef returnType = null;
        for (Expression bodyExpr: body) {
            try {
                TypeRef bodyTypeRef = new TypeRef();
                bodyExpr.unify(bodyTypeRef, bindEnv);
                returnType = bodyTypeRef;
            } catch (UnifyException exc) {
                throw UnifyException.addCause("regex-bind body", exc);
            }
        }

        AbstractType returnOption = new AbstractType("option", Arrays.asList(returnType));
        typeRef.unify(new TypeRef(returnOption));
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("regex-bind"));
        scheml.add(targetExpression.toScheml());
        scheml.add(new StringExpr(regex));
        List<Expression> varsList = new ArrayList<>();
        for (RegexBindVariable v: vars) {
            if (v.processingFunc != null) {
                List<Expression> procFuncList = new ArrayList<>();
                procFuncList.add(v.processingFunc.toScheml());
                procFuncList.add(new SymbolLiteralExpr(v.name));
                varsList.add(new ListExpr(procFuncList));
            } else {
                varsList.add(new SymbolLiteralExpr(v.name));
            }
        }
        scheml.add(new ListExpr(varsList));
        for (Expression bodyExpr: body) {
            scheml.add(bodyExpr.toScheml());
        }

        return new ListExpr(scheml);
    }

    public static class RegexBindVariable {
        public String name;
        public Expression processingFunc;

        public RegexBindVariable(String name) {
            this.name = name;
            this.processingFunc = null;
        }

        public RegexBindVariable(String name, Expression processingFunc) {
            this.name = name;
            this.processingFunc = processingFunc;
        }
    }
}
