package edu.vanderbilt.cs.wutkam.scheme.type;

import java.util.List;

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
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(typeName);
        for (TypeRef typeParameter: typeParameters) {
            builder.append(" ");
            builder.append(typeParameter.getType().toString());
        }
        return builder.toString();
    }

    @Override
    public String toSignatureString(TypeSymbolGenerator gen) {
        StringBuilder builder = new StringBuilder();
        builder.append(typeName);
        for (TypeRef typeRef: typeParameters) {
            builder.append(" ");
            Type type = typeRef.getType();
            boolean parenthesize = (type instanceof FunctionType) || (type instanceof ConsType) ||
                    (type instanceof AbstractType);
            if (parenthesize) builder.append("(");
            builder.append(typeRef.getType().toSignatureString(gen));
            if (parenthesize) builder.append(")");
        }
        return builder.toString();
    }
}
