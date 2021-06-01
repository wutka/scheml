package edu.vanderbilt.cs.wutkam.scheme.type;

import java.util.Map;

public abstract class Type {
    @Override
    public String toString() {
        return toSignatureString(new TypeSymbolGenerator());
    }

    public abstract String toSignatureString(TypeSymbolGenerator gen);
}
