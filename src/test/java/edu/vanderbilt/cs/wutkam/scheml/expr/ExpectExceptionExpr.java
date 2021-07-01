package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.VoidType;

import java.util.List;

import static org.junit.jupiter.api.Assertions.fail;

/** Implements a try-catch block to assert that a test throws an exception */
public class ExpectExceptionExpr implements Expression {

    protected String exceptionName;
    protected String message;
    protected List<Expression> body;

    public ExpectExceptionExpr(String exceptionName, String message, List<Expression> body) {
        this.exceptionName = exceptionName;
        this.message = message;
        this.body = body;
    }
    
    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        try {
            for (Expression expr: body) {
                expr.evaluate(env, false);
            }
            if (message == null) {
                fail("Expected exception "+exceptionName);
            } else {
                fail(message);
            }
        } catch (Throwable exc) {
            if (!exc.getClass().getSimpleName().equals(exceptionName)) {
                if (message == null) {
                    fail("Expected exception " + exceptionName);
                } else {
                    fail(message);
                }
            }
        }
        return new VoidExpr();
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {

        for (Expression expr: body) {
            TypeRef exprType = new TypeRef();
            expr.unify(exprType, env);
        }

        TypeRef thisType = new TypeRef(VoidType.TYPE);
        typeRef.unify(thisType);
    }
}
