package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 6/2/21
 * Time: 11:21 AM
 */
public interface Applicable {
    Expression apply(List<Expression> arguments, Environment<Expression> env) throws LispException;
}
