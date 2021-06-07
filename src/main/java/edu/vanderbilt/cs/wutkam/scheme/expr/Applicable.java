package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

import java.util.List;

/** Represents an expression that can be applied like a function
 */
public interface Applicable {
    Expression apply(List<Expression> arguments, Environment<Expression> env) throws LispException;
}
