package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.ArrayList;

/** A simple double type */
public class DoubleType extends Type {
    public static Type TYPE = new DoubleType();

    public String toSignatureString(TypeSymbolGenerator symgen) { return "double"; }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "DoubleType", new ArrayList<>());
    }
}