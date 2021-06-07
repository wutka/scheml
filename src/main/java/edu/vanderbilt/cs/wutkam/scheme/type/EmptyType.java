package edu.vanderbilt.cs.wutkam.scheme.type;

/** An empty type is one whose type is unknown, but which has a unique identifier so that two
 * instances of EmptyType with the same unique identifier should ultimately have the same type
 * whenever it is discovered what that type should be
 */
public class EmptyType extends Type {
    protected static long nextId = 0;
    protected String id;

    public synchronized static String generateId() {
        return "id"+(nextId++);
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
