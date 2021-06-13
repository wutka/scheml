package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.List;

/** A block of statements that is similar to a let statement, only statements can be intermixed where
 * some just do something (usually some kind of I/O), while some bind a variable using the (:= binding expr) form
 */
public class StatementsExpr implements Expression {
    List<Expression> statements;

    public StatementsExpr(List<Expression> statements) {
        this.statements = statements;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        Expression last = null;

        // Evaluate each statement
        Environment<Expression> statementEnv = new Environment<>(env);
        for (int i=0; i < statements.size(); i++) {
            Expression statement = statements.get(i);

            // The last statement is in the tail position
            last = statement.evaluate(statementEnv, (i == statements.size()-1) && inTailPosition);
        }
        return last;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        Environment<TypeRef> statementEnv = new Environment<>(env);
        TypeRef last = null;
        // Unify each statement
        for (int i=0; i < statements.size(); i++) {
            Expression statement = statements.get(i);

            // We reset the type ref each time because there is no requirement that each
            // statement has the same type
            last = new TypeRef();
            statement.unify(last, statementEnv);
        }
        typeRef.unify(last);
    }
}
