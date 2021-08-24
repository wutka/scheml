package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.ArrayList;

/** A simple char type */
public class CharType extends Type {
    public static Type TYPE = new CharType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "char"; }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "CharType", new ArrayList<>());
    }
}
