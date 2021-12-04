package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.ArrayList;

/** A simple bignum type */
public class BignumType extends Type {
    public static Type TYPE = new BignumType();

    public String toSignatureString(TypeSymbolGenerator symgen) { return "bignum"; }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "BignumType", new ArrayList<>());
    }
}