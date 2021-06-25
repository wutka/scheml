package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;

/** Represents a unary function that takes one argument of type A and returns a value of type B */
public interface UnaryFunction<A,B> {
    B apply(A a) throws LispException;
}
