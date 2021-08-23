package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.LispException;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/** Represents the type of an abstract type */
public class AbstractType extends Type {
    /** The name of the type */
    public String typeName;

    /** The type parameters for the type */
    public List<TypeRef> typeParameters;

    public AbstractType(String typeName, List<TypeRef> typeParameters) {
        this.typeName = typeName;
        this.typeParameters = typeParameters;
    }

    public AbstractType(AbstractTypeDecl decl) {
        this.typeName = decl.typeName;
        this.typeParameters = decl.parametricTypes;
    }

    @Override
    public Type copy(Map<String,TypeRef> linkageMap) {
        List<TypeRef> typeParameters = new ArrayList<>();
        // Copy all the abstract type's type parameters
        for (TypeRef typeRef: this.typeParameters) {
            typeParameters.add(typeRef.copy(linkageMap));
        }
        // Return a ref to the new copy of the abstract type
        return new AbstractType(typeName, typeParameters);
    }

    @Override
    public void unify(Type other) throws LispException {
        // Make sure they are both abstract types
        if (!(other instanceof AbstractType)) {
            throw new UnifyException("Unable to unify " + this + " with " + other);
        }
        AbstractType otherType = (AbstractType) other;

        // with the same type name
        if (!typeName.equals(otherType.typeName)) {
            throw new UnifyException("Unable to unify " + this + " with " + other);
        }

        // and same number of parametric types
        if (typeParameters.size() != otherType.typeParameters.size()) {
            throw new UnifyException("Unable to unify " + this + " with " + other);
        }

        // unify the parametric types
        for (int i=0; i < typeParameters.size(); i++) {
            try {
                typeParameters.get(i).unify(otherType.typeParameters.get(i));
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Unable to unify " + this + " with " +
                        other + " because type parameters don't match", exc);
            }
        }
    }

    @Override
    public String toSignatureString(TypeSymbolGenerator gen) {
        StringBuilder builder = new StringBuilder();
        builder.append(typeName);
        for (TypeRef typeRef: typeParameters) {
            builder.append(" ");
            Type type = typeRef.getType();
            boolean parenthesize = (type instanceof FunctionType) ||
                    ((type instanceof AbstractType) && (((AbstractType)type).typeParameters.size() > 0));
            if (parenthesize) builder.append("(");
            builder.append(typeRef.getType().toSignatureString(gen));
            if (parenthesize) builder.append(")");
        }
        return builder.toString();
    }
}
