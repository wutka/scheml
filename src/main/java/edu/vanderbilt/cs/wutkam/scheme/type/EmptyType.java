package edu.vanderbilt.cs.wutkam.scheme.type;

public class EmptyType extends Type {
    protected static long nextId = 0;
    protected String id;

    public synchronized static String generateId() {
        return "id"+Long.toString(nextId++);
    }

    public EmptyType() {
        this.id = generateId();
    }

    public EmptyType(String id) {
        this.id = id;
    }

    public String toSignatureString(TypeSymbolGenerator symGen) {
        return symGen.generateSymbol(this.id);
    }

    @Override
    public boolean equals(Object otherObj) {
        if (!(otherObj instanceof EmptyType)) return false;
        EmptyType other = (EmptyType) otherObj;
        return id.equals(other.id);
    }
}
