package edu.vanderbilt.cs.wutkam.scheme.type;

/** Represents a node in a linked list. Each element in the list must be the same type
 * and this object keeps a type reference to its element type
 */
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
