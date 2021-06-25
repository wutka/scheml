package edu.vanderbilt.cs.wutkam.scheml.type;

/** A simple double type */
public class DoubleType extends Type {
    public static Type TYPE = new DoubleType();

    public String toSignatureString(TypeSymbolGenerator symgen) { return "double"; }
}
