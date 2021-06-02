package edu.vanderbilt.cs.wutkam.scheme.type;

public class CharType extends Type {
    public static Type TYPE = new CharType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "char"; }
}