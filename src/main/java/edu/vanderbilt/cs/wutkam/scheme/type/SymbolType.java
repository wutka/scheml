package edu.vanderbilt.cs.wutkam.scheme.type;

/** A simple symbol type */
public class SymbolType extends Type {
    public static Type TYPE = new SymbolType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "string"; }
}
