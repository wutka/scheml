package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.Arrays;
import java.util.Map;

/** Represents the type of an abstract type */
public class DictType extends Type {
    /** The type parameters for the type */
    public TypeRef keyTypeParameter;
    public TypeRef valueTypeParameter;

    public DictType(TypeRef keyTypeParameter, TypeRef valueTypeParameter) {
        this.keyTypeParameter = keyTypeParameter;
        this.valueTypeParameter = valueTypeParameter;
    }

    @Override
    public Type copy(Map<String,TypeRef> linkageMap) {
        return new DictType(keyTypeParameter.copy(linkageMap),
                valueTypeParameter.copy(linkageMap));
    }

    @Override
    public void unify(Type other) throws LispException {
        // Make sure they are both abstract types
        if (!(other instanceof DictType)) {
            throw new UnifyException("Unable to unify " + this + " with " + other);
        }
        DictType otherType = (DictType) other;

        try {
            keyTypeParameter.unify(otherType.keyTypeParameter);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Unable to unify " + this + " with " +
                    other + " because key type parameters don't match", exc);
        }

        try {
            valueTypeParameter.unify(otherType.valueTypeParameter);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Unable to unify " + this + " with " +
                    other + " because key type parameters don't match", exc);
        }
    }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "DictType",
                Arrays.asList(keyTypeParameter.getType().toTypeADT(gen),
                        valueTypeParameter.getType().toTypeADT(gen)));
    }

    @Override
    public String toSignatureString(TypeSymbolGenerator gen) {
        StringBuilder builder = new StringBuilder();
        builder.append("dict");
        builder.append(" ");
        builder.append(keyTypeParameter.getType().toSignatureString(gen));
        builder.append(" ");
        builder.append(valueTypeParameter.getType().toSignatureString(gen));
        return builder.toString();
    }
}