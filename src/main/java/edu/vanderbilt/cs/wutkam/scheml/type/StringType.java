package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.ArrayList;

/** A simple string type */
public class StringType extends Type {
    public static Type TYPE = new StringType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "string"; }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "StringType", new ArrayList<>());
    }
}
