package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.LispException;

public class TypeRef {
    TypeHolder typeHolder;

    public TypeRef() {
        Type type = new EmptyType();
        typeHolder = new TypeHolder(type);
    }

    public TypeRef(Type type) {
        typeHolder = new TypeHolder(type);
    }

    public boolean isFull() {
        return !(typeHolder.getType() instanceof EmptyType);
    }

    public Type getType() {
        return typeHolder.getType();
    }

    public void setType(Type other) throws LispException {
        if (!(typeHolder.getType() instanceof EmptyType)) {
            throw new LispException("Tried to set type in full TypeRef");
        }
        this.typeHolder.setType(other);
    }

    public void unify(TypeRef other) throws LispException {
        if (isFull()) {
            if (other.isFull()) {
                if (getType() instanceof ConsType) {
                    if (!(other.getType() instanceof ConsType)) {
                        throw new UnifyException("Unable to unify " + getType() + " with " + other.getType());
                    }
                    ConsType thisCons = (ConsType) getType();
                    ConsType otherCons = (ConsType) other.getType();
                    try {
                        thisCons.elementType.unify(otherCons.elementType);
                    } catch (UnifyException exc) {
                        throw UnifyException.addCause("Unable to unify cons element types", exc);
                    }
                } else if (getType() instanceof FunctionType) {
                    if (!(other.getType() instanceof FunctionType)) {
                        throw new UnifyException("Unable to unify " + getType() + " with " + other.getType());
                    }
                    FunctionType thisFunc = (FunctionType) getType();
                    FunctionType otherFunc = (FunctionType) other.getType();

                    if (thisFunc.arity != otherFunc.arity) {
                        throw new UnifyException("Can't unify function " + getType() +
                                " with different arity function " + other.getType());
                    }

                    for (int i=0; i < thisFunc.arity; i++) {
                        try {
                            thisFunc.paramTypes[i].unify(otherFunc.paramTypes[i]);
                        } catch (UnifyException exc) {
                            throw UnifyException.addCause("Can't unify parameter "+i+" of "+getType()+" with "+
                                    other.getType(), exc);
                        }
                    }

                    try {
                        thisFunc.returnType.unify(otherFunc.returnType);
                    } catch (UnifyException exc) {
                        throw UnifyException.addCause("Can't unify return type of "+getType()+
                                " with "+other.getType(), exc);
                    }
                } else if (getType() != other.getType()) {
                    throw new UnifyException("Can't unify "+getType()+" with "+getType());
                }
            } else {
                other.setType(getType());
            }
        } else {
            this.setType(other.getType());
        }
    }
}
