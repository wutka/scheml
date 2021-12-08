package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;

import java.util.Arrays;
import java.util.Map;

/** Represents the type of an abstract type */
public class SetType extends Type {
    /** The type parameters for the type */
    public TypeRef keyTypeParameter;

    public SetType(TypeRef keyTypeParameter) {
        this.keyTypeParameter = keyTypeParameter;
    }

    @Override
    public Type copy(Map<String,TypeRef> linkageMap) {
        return new SetType(keyTypeParameter.copy(linkageMap));
    }

    @Override
    public void unify(Type other) throws LispException {
        // Make sure they are both abstract types
        if (!(other instanceof SetType)) {
            throw new UnifyException("Unable to unify " + this + " with " + other);
        }
        SetType otherType = (SetType) other;

        try {
            keyTypeParameter.unify(otherType.keyTypeParameter);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Unable to unify " + this + " with " +
                    other + " because key type parameters don't match", exc);
        }
    }

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        return new AbstractTypeExpr("type-val", "SetType",
                Arrays.asList(keyTypeParameter.getType().toTypeADT(gen)));
    }

    @Override
    public String toSignatureString(TypeSymbolGenerator gen) {
        StringBuilder builder = new StringBuilder();
        builder.append("set");
        builder.append(" ");
        builder.append(keyTypeParameter.getType().toSignatureString(gen));
        return builder.toString();
    }
}