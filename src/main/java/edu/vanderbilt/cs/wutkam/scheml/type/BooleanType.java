package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.ArrayList;

/** A simple boolean type */
public class BooleanType extends Type {
    public static Type TYPE = new BooleanType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "bool"; }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "BoolType", new ArrayList<>());
    }
}
