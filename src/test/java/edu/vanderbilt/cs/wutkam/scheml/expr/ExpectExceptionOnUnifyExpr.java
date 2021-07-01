package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.VoidType;

import java.util.List;

import static org.junit.jupiter.api.Assertions.fail;

/** Implements a try-catch block to assert that a test throws an exception */
public class ExpectExceptionOnUnifyExpr implements Expression {

    protected String exceptionName;
    protected String message;
    protected List<Expression> body;

    public ExpectExceptionOnUnifyExpr(String exceptionName, String message, List<Expression> body) {
        this.exceptionName = exceptionName;
        this.message = message;
        this.body = body;
    }
    
    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        // Since we were expecting an exception when unifying, don't try to evaluate
        return new VoidExpr();
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {

        try {
            for (Expression expr: body) {
                TypeRef exprType = new TypeRef();
                expr.unify(exprType, env);
            }

            TypeRef thisType = new TypeRef(VoidType.TYPE);
            typeRef.unify(thisType);
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
    }
}
