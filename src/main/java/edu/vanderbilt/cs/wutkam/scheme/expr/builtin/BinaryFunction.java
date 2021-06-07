package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;

/** A binary function taking types A and B and returning type C */
public interface BinaryFunction<A,B,C> {
    C apply(A a, B b) throws LispException;
}
