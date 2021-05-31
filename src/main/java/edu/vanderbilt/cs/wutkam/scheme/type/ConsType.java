package edu.vanderbilt.cs.wutkam.scheme.type;

public class ConsType extends Type {
    public TypeRef elementType;

    public ConsType() {
        elementType = new TypeRef();
    }

    public ConsType(Type type) {
        elementType = new TypeRef(type);
    }

    public ConsType copy() {
        return new ConsType(elementType.getType().copy());
    }
    public String toString() {
        if (!elementType.isFull()) {
            return "cons 'a";
        } else {
            return "cons "+elementType.getType().toString();
        }
    }
}
