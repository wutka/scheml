package edu.vanderbilt.cs.wutkam.scheme.type;

public class DoubleType extends Type {
    public static Type TYPE = new DoubleType();

    public String toSignatureString(TypeSymbolGenerator symgen) { return "double"; }
}