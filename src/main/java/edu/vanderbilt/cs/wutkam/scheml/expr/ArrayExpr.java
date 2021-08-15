package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.ArrayType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.ArrayList;
import java.util.List;

public class ArrayExpr implements Expression {
    public Expression[] values;

    public ArrayExpr(int size, Expression defaultValue) {
        this.values = new Expression[size];
        for (int i=0; i < size; i++) this.values[i] = defaultValue;
    }

    public ArrayExpr(ListExpr listExpr) {
        this.values = new Expression[listExpr.size()];
        for (int i=0; i < this.values.length; i++) {
            this.values[i] = listExpr.getElement(i);
        }
    }

    public ArrayExpr(Expression[] values) {
        this.values = values;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        if (values.length > 0) {
            TypeRef elemType = new TypeRef();
            values[0].unify(elemType, env);
            typeRef.unify(new TypeRef(new ArrayType(elemType)));
        } else {
            typeRef.unify(new TypeRef(new ArrayType(new TypeRef())));
        }
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("make-array"));
        for (int i=0; i < values.length; i++) {
            scheml.add(values[i].toScheml());
        }
        return new ListExpr(scheml);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("(make-array");
        for (int i=0; i < values.length; i++) {
            builder.append(' ');
            builder.append(values[i].toString());
        }
        builder.append(')');
        return builder.toString();
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof ArrayExpr)) return false;
        ArrayExpr otherArray = (ArrayExpr) other;
        if (values.length != otherArray.values.length) return false;
        for (int i=0; i < values.length; i++) {
            if (!values[i].equals(otherArray.values[i])) return false;
        }
        return true;
    }
}
