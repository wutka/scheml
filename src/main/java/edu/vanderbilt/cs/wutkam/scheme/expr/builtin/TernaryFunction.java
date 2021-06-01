package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;

public interface TernaryFunction<A,B,C,D> {
    D apply(A a, B b, C c) throws LispException;
}
