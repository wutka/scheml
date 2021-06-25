package edu.vanderbilt.cs.wutkam.scheml.type;

/** A simple string type */
public class StringType extends Type {
    public static Type TYPE = new StringType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "string"; }
}
