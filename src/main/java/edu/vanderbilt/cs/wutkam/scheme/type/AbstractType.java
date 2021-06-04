package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;

import java.util.List;
import java.util.Map;

public class AbstractType extends Type {
    public String typeName;
    public List<TypeRef> typeParameters;
    public Map<String,FunctionType> typeConstructors;

    public AbstractType(String typeName, List<TypeRef> typeParameters) {
        this.typeName = typeName;
    }

    public AbstractType(String typeName, List<TypeRef> typeParameters,
                        Map<String,FunctionType> typeConstructors) {
        this.typeName = typeName;
        this.typeConstructors = typeConstructors;
    }

    public void addTypeConstructors(Map<String,FunctionType> typeConstructors) {
        this.typeConstructors = typeConstructors;
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
