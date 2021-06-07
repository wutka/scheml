package edu.vanderbilt.cs.wutkam.scheme.type;

/** A simple void type */
public class VoidType extends Type {
    public static Type TYPE = new VoidType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "void"; }
}
