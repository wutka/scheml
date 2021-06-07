package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;

/** Represents a unary function that takes one argument of type A and returns a value of type B */
public interface UnaryFunction<A,B> {
    public B apply(A a) throws LispException;
}
