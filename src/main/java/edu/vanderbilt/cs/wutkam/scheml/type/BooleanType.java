package edu.vanderbilt.cs.wutkam.scheml.type;

/** A simple boolean type */
public class BooleanType extends Type {
    public static Type TYPE = new BooleanType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "bool"; }
}
