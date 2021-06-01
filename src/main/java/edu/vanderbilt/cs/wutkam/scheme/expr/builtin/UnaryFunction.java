package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

public interface UnaryFunction<A,B> {
    public B apply(A a) throws LispException;
}
