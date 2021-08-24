package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/** Represents the type of an abstract type */
public class ArrayType extends Type {
    /** The type parameters for the type */
    public TypeRef typeParameter;

    public ArrayType(TypeRef typeParameter) {
        this.typeParameter = typeParameter;
    }

    @Override
    public Type copy(Map<String,TypeRef> linkageMap) {
        return new ArrayType(typeParameter.copy(linkageMap));
    }

    @Override
    public void unify(Type other) throws LispException {
        // Make sure they are both abstract types
        if (!(other instanceof ArrayType)) {
            throw new UnifyException("Unable to unify " + this + " with " + other);
        }
        ArrayType otherType = (ArrayType) other;

        try {
            typeParameter.unify(otherType.typeParameter);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Unable to unify " + this + " with " +
                    other + " because type parameters don't match", exc);
        }
    }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "ArrayType",
                Arrays.asList(typeParameter.getType().toTypeADT(gen)));
    }

    @Override
    public String toSignatureString(TypeSymbolGenerator gen) {
        StringBuilder builder = new StringBuilder();
        builder.append("array");
        builder.append(" ");
        builder.append(typeParameter.getType().toSignatureString(gen));
        return builder.toString();
    }
}