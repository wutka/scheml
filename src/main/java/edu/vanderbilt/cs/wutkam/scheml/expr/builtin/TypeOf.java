package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeSymbolGenerator;

/** Prints a string to stdout
 */
public class TypeOf extends BuiltinFunctionExpr {
    public TypeOf(String name) {
        super(name, "'a -> type-val");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        Expression expr = args[0];
        TypeRef exprType = new TypeRef();
        expr.unify(exprType, new Environment<>());
        return exprType.getType().toTypeADT(new TypeSymbolGenerator());
    }
}
