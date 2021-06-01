package edu.vanderbilt.cs.wutkam.scheme.type;

import java.util.HashMap;

public class ConsType extends Type {
    public TypeRef elementType;

    public ConsType() {
        elementType = new TypeRef();
    }

    public ConsType(Type type) {
        elementType = new TypeRef(type);
    }

    public ConsType(TypeRef typeRef) {
        this.elementType = typeRef;
    }

    public String toSignatureString(TypeSymbolGenerator symGen) {
        return "cons "+elementType.getType().toSignatureString(symGen);
    }
}
