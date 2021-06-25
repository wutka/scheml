package edu.vanderbilt.cs.wutkam.scheml.type;

/** A simple integer type */
public class IntType extends Type {
    public static Type TYPE = new IntType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "int"; }
}
