package edu.vanderbilt.cs.wutkam.scheme.type;

import java.util.Map;

public class FunctionType extends Type {
    public int arity;
    public TypeRef returnType;
    public TypeRef[] paramTypes;

    public FunctionType(int arity) {
        this.arity = arity;
        returnType = new TypeRef();
        paramTypes = new TypeRef[arity];
        for (int i=0; i < arity; i++) paramTypes[i] = new TypeRef();
    }

    public FunctionType(int arity, TypeRef[] paramTypes, TypeRef returnType) {
        this.arity = arity;
        this.paramTypes = paramTypes;
        this.returnType = returnType;
    }

    @Override
    public String toSignatureString(TypeSymbolGenerator symGen) {
        boolean first = true;
        StringBuilder builder = new StringBuilder();
        for (int i=0; i < arity; i++) {
            if (!first) builder.append(" -> ");
            if (paramTypes[i].getType() instanceof FunctionType) {
                builder.append("(");
                builder.append(paramTypes[i].getType().toSignatureString(symGen));
                builder.append(")");
            } else {
                builder.append(paramTypes[i].getType().toSignatureString(symGen));
            }
            first = false;
        }
        if (arity == 0) builder.append("void");

        builder.append(" -> ");
        if (returnType.getType() instanceof FunctionType) {
            builder.append("(");
            builder.append(returnType.getType().toSignatureString(symGen));
            builder.append(")");

        } else {
            builder.append(returnType.getType().toSignatureString(symGen));
        }

        return builder.toString();
    }
}
