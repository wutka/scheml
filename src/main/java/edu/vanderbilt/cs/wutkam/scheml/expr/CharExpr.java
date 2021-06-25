package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.CharType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.Objects;

/** Represents a character value whose values are represented as #\ch where ch is the character
 * (e.g. #\a  #\b  #\c)
 */
public class CharExpr implements Expression {
    public final char value;

    public CharExpr(char value) {
        this.value = value;
    }

    @Override
    public Object toJavaValue() { return value; }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        TypeRef myType = new TypeRef(CharType.TYPE);
        typeRef.unify(myType);
    }

    @Override
    public String toString() {
        return "#\\"+value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CharExpr charExpr = (CharExpr) o;
        return value == charExpr.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}
