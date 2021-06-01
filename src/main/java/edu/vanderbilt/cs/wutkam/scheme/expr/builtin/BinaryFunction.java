package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

public interface BinaryFunction<A,B,C> {
    C apply(A a, B b) throws LispException;
}
