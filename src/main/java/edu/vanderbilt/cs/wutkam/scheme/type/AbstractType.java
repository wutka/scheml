package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;

import java.util.List;

public class AbstractType extends Type {
    public String typeName;
    public List<TypeRef> parametricTypes;
    public List<TypeConstructorExpr> typeConstructors;

    public AbstractType(String typeName, List<TypeRef> parametricTypes) {
        this.typeName = typeName;
        this.parametricTypes = parametricTypes;
    }

    @Override
    public String toSignatureString(TypeSymbolGenerator gen) {
        StringBuilder builder = new StringBuilder();
        builder.append(typeName);
        for (TypeRef typeRef: parametricTypes) {
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
