package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;

/** Represents a ternary function that takes three arguments of types A, B, and C and returns a value of type D */
public interface TernaryFunction<A,B,C,D> {
    D apply(A a, B b, C c) throws LispException;
}
