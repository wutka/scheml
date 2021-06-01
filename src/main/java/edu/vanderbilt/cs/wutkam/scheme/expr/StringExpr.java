package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.StringType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.Objects;

public class StringExpr implements Expression {
    public String value;

    public StringExpr(String value) {
        this.value = value;
    }

    @Override
    public Object toJavaValue() { return value; }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef myType = new TypeRef(StringType.TYPE);
        typeRef.unify(myType);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int i=0; i < value.length(); i++) {
            char ch = value.charAt(i);
            if (ch == '"') {
                builder.append("\\\"");
            } else if (ch == '\n') {
                builder.append("\\n");
            } else if (ch == '\r') {
                builder.append("\\r");
            } else if (ch == '\t') {
                builder.append("\\t");
            } else {
                builder.append(ch);
            }
        }
        builder.append('"');
        return builder.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        StringExpr that = (StringExpr) o;
        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}
