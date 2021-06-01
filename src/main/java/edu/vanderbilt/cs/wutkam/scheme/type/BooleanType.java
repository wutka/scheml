package edu.vanderbilt.cs.wutkam.scheme.type;

import java.util.Map;

public class BooleanType extends Type {
    public static Type TYPE = new BooleanType();

    public String toSignatureString(TypeSymbolGenerator symGen) { return "bool"; }
}
