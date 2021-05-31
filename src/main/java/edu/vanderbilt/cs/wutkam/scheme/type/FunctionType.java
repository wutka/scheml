package edu.vanderbilt.cs.wutkam.scheme.type;

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

    public FunctionType copy() {
        TypeRef[] newParamTypes = new TypeRef[arity];
        for (int i=0; i < arity; i++) {
            newParamTypes[i] = new TypeRef(paramTypes[i].getType().copy());
        }
        TypeRef newReturnType = new TypeRef(returnType.getType().copy());

        return new FunctionType(arity, newParamTypes, newReturnType);
    }

    @Override
    public String toString() {
        String[] typeParams = new String[arity];
        char funcChar = 'a';

        StringBuilder builder = new StringBuilder();

        boolean first = true;
        for (int i=0; i < arity; i++) {
            if (!paramTypes[i].isFull()) {
                boolean found = false;
                for (int j = 0; j < i; j++) {
                    if (paramTypes[j].getType().equals(paramTypes[i].getType())) {
                        typeParams[i] = typeParams[j];
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    typeParams[i] = "'" + funcChar;
                    if (funcChar == 'z') {
                        funcChar = 'A';
                    } else {
                        funcChar++;
                    }
                    if (!first) builder.append(" -> ");
                }
                if (!first) builder.append(" -> ");
                builder.append(typeParams[i]);
            } else {
                if (!first) builder.append(" -> ");
                if (paramTypes[i].getType() instanceof FunctionType) {
                    builder.append("(");
                    builder.append(paramTypes[i].toString());
                    builder.append(")");
                } else {
                    builder.append(paramTypes[i].getType().toString());
                }
            }
            first = false;
        }

        if (arity == 0) {
            builder.append(VoidType.TYPE.toString());
        }

        builder.append(" -> ");
        boolean found = false;
        if (!returnType.isFull()) {
            for (int i=0; i < arity; i++) {
                if (paramTypes[i].getType().equals(returnType.getType())) {
                    builder.append(typeParams[i]);
                    found = true;
                }
            }
            if (!found) {
                builder.append("'");
                builder.append(funcChar);
            }
        } else {
            builder.append(returnType.getType().toString());
        }

        return builder.toString();
    }
}
