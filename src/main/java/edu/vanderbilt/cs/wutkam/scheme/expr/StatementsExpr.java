package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.List;

/**
 */
public class StatementsExpr implements Expression {
    List<Expression> statements;

    public StatementsExpr(List<Expression> statements) {
        this.statements = statements;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Expression last = null;
        Environment<Expression> statementEnv = new Environment<>(env);
        for (int i=0; i < statements.size(); i++) {
            Expression statement = statements.get(i);
            last = statement.evaluate(statementEnv, (i == statements.size()-1) && inTailPosition);
        }
        return last;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        Environment<TypeRef> statementEnv = new Environment<>(env);
        TypeRef last = null;
        for (int i=0; i < statements.size(); i++) {
            Expression statement = statements.get(i);
            last = new TypeRef();
            statement.unify(last, statementEnv);
        }
        typeRef.unify(last);
    }
}
