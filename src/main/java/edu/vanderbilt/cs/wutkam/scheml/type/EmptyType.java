package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;

import java.util.ArrayList;
import java.util.Arrays;

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

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "AnyType",
                Arrays.asList(new StringExpr(gen.generateSymbol(id))));
    }
}
