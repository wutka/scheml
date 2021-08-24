package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.ArrayList;

/** A simple symbol type */
public class SymbolType extends Type {
    public static Type TYPE = new SymbolType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "symbol"; }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "SymbolType", new ArrayList<>());
    }
}
