package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.FunctionExpr;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/** Hold the type of a function, which consists of an arity, the types of each parameter and a return type */
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
        if ((arity == 1) && paramTypes[0].getType().equals(VoidType.TYPE)) {
            this.arity = 0;
            this.paramTypes = new TypeRef[0];
        } else {
            this.arity = arity;
            this.paramTypes = paramTypes;
        }
        this.returnType = returnType;
    }

    public FunctionType(FunctionExpr functionExpr) {
        this.arity = functionExpr.arity;
        this.paramTypes = functionExpr.paramTypes;
        this.returnType = functionExpr.returnType;
    }

    @Override public Type copy(Map<String,TypeRef> linkageMap) {
        // Copy all the param types and return types of this function
        TypeRef[] paramTypes = new TypeRef[arity];
        for (int i = 0; i < paramTypes.length; i++) {
            paramTypes[i] = this.paramTypes[i].copy(linkageMap);
        }
        TypeRef returnType = this.returnType.copy(linkageMap);
        // Return a reference to a new function type instance that uses the new copies
        return new FunctionType(paramTypes.length, paramTypes, returnType);
    }

    @Override
    public void unify(Type other) throws LispException {
        // Make sure they are both functions
        if (!(other instanceof FunctionType)) {
            throw new UnifyException("Unable to unify " + this + " with " + other.toString());
        }
        FunctionType otherFunc = (FunctionType) other;

        // Make sure the arity matches
        if (arity != otherFunc.arity) {
            throw new UnifyException("Can't unify function " + this +
                    " with different arity function " + other);
        }

        for (int i = 0; i < arity; i++) {
            try {
                // Unify the param types
                paramTypes[i].unify(otherFunc.paramTypes[i]);
            } catch (UnifyException exc) {
                throw UnifyException.addCause("Can't unify parameter " + i + " of " + this + " with " +
                        other, exc);
            }
        }

        // Unify the return types
        try {
            returnType.unify(otherFunc.returnType);
        } catch (UnifyException exc) {
            throw UnifyException.addCause("Can't unify return type of " + this +
                    " with " + other, exc);
        }
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

    @Override
    public AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen) {
        if (paramTypes.length == 0) {
            return new AbstractTypeExpr("type-val", "FunctionType",
                    Arrays.asList(VoidType.TYPE.toTypeADT(gen),
                            returnType.getType().toTypeADT(gen)));
        } else {
            List<AbstractTypeExpr> types = new ArrayList<>();
            for (int i=0; i < paramTypes.length; i++) {
                types.add(paramTypes[i].getType().toTypeADT(gen));
            }
            AbstractTypeExpr returnTypeVal = returnType.getType().toTypeADT(gen);

            AbstractTypeExpr curr =
                    new AbstractTypeExpr("type-val", "FunctionType",
                     Arrays.asList(types.get(types.size()-1), returnTypeVal));

            for (int i=paramTypes.length-2; i >= 0; i--) {
                curr = new AbstractTypeExpr("type-val", "FunctionType",
                        Arrays.asList(types.get(i), curr));
            }
            return curr;
        }
    }
}
