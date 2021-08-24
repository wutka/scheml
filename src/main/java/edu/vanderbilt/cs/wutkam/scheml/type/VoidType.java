package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.ArrayList;

/** A simple void type */
public class VoidType extends Type {
    public static Type TYPE = new VoidType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "void"; }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "VoidType", new ArrayList<>());
    }
}
