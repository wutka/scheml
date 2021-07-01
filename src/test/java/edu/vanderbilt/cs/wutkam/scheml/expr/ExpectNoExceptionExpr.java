package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.VoidType;

import java.util.List;

import static org.junit.jupiter.api.Assertions.fail;

/** Implements a try-catch block to assert that a test throws an exception */
public class ExpectNoExceptionExpr implements Expression {

    protected String message;
    protected List<Expression> body;

    public ExpectNoExceptionExpr(String message, List<Expression> body) {
        this.message = message;
        this.body = body;
    }
    
    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        try {
            for (Expression expr: body) {
                expr.evaluate(env, false);
            }
        } catch (Throwable exc) {
            if (message == null) {
                fail("Unexpected exception " + exc.getClass().getSimpleName());
            } else {
                fail(message);
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
