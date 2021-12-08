package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.DictType;
import edu.vanderbilt.cs.wutkam.scheml.type.SetType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.*;

public class SetExpr implements Expression {
    public Set<Expression> set;

    public SetExpr() {
        this.set = new HashSet<>();
    }

    public SetExpr(Set<Expression> otherSet) {
        this.set = otherSet;
    }

    public SetExpr(ListExpr listExpr) throws LispException {
        this.set = new HashSet<>();

        this.set.addAll(listExpr.elementsFrom(0));
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        if (set.size() > 0) {
            for (Expression elem: set) {
                TypeRef elemType = new TypeRef();
                elem.unify(elemType, env);
                typeRef.unify(new TypeRef(new SetType(elemType)));
            }
        } else {
            typeRef.unify(new TypeRef(new SetType(new TypeRef())));
        }
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("make-set"));
        scheml.addAll(set);
        return new ListExpr(scheml);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("(make-set");
        for (Expression elem: set) {
            builder.append(" ");
            builder.append(elem.toString());
        }
        builder.append(')');
        return builder.toString();
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof SetExpr)) return false;
        SetExpr otherSet = (SetExpr) other;
        return set.equals(otherSet.set);
    }
}
