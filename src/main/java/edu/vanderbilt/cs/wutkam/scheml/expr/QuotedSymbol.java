package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.SymbolType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

public class QuotedSymbol implements Expression {
    String value;

    public QuotedSymbol(String value) {
        this.value = value;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        typeRef.unify(new TypeRef(SymbolType.TYPE));
    }

    @Override
    public String toString() {
        return value;
    }
}
