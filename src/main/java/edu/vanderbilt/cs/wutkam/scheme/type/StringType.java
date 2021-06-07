package edu.vanderbilt.cs.wutkam.scheme.type;

/** A simple string type */
public class StringType extends Type {
    public static Type TYPE = new StringType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "string"; }
}
