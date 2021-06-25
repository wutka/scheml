package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.List;

/** Represents a cons expression generated from the (list) form. Because the form doesn't evaluate the
 * arguments, this expression becomes the way the list is converted into a ConsExpr with evaluated
 * values.
 */
public class GeneratedConsExpr implements Expression {
    public final List<Expression> expressions;

    public GeneratedConsExpr(List<Expression> expressions) {
        this.expressions = expressions;
    }

    @Override
    public Expression evaluate(Environment<Expression> env, boolean inTailPosition) throws LispException {
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        // Build the cons list in reverse starting and null and prepending each expression value
        for (int i=expressions.size()-1; i >= 0; i--) {
            curr = ConsTypeDecl.newCons(expressions.get(i).evaluate(env, false), curr);
        }
        return curr;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        // Make sure that each element in the list has the same type
        TypeRef elementType = new TypeRef();
        for (Expression expr: expressions) {
            expr.unify(elementType, env);
        }

        // Unify with the requested type ref
        typeRef.unify(new TypeRef(ConsTypeDecl.newConsType(elementType)));
    }
}
