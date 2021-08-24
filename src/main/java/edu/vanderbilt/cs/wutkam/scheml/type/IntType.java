package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.ArrayList;

/** A simple integer type */
public class IntType extends Type {
    public static Type TYPE = new IntType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "int"; }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "IntType", new ArrayList<>());
    }
}
