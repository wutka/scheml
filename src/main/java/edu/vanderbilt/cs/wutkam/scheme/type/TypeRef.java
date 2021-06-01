package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.LispException;

import java.util.Map;

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

    public TypeRef copy(Map<String,TypeRef> linkageMap) {
        if (typeHolder.type instanceof EmptyType) {
            EmptyType thisType = (EmptyType) typeHolder.type;
            TypeRef newCopy = linkageMap.get(thisType.id);
            if (newCopy == null) {
                newCopy = new TypeRef();
                linkageMap.put(thisType.id, newCopy);
            }
            return newCopy;
        }
        else if (typeHolder.type instanceof ConsType) {
            ConsType thisType = (ConsType) typeHolder.type;
            return new TypeRef(new ConsType(thisType.elementType.copy(linkageMap)));
        } else if (typeHolder.type instanceof FunctionType) {
            FunctionType thisType = (FunctionType) typeHolder.type;
            TypeRef[] paramTypes = new TypeRef[thisType.arity];
            for (int i=0; i < paramTypes.length; i++) {
                paramTypes[i] = thisType.paramTypes[i].copy(linkageMap);
            }
            TypeRef returnType = thisType.returnType.copy(linkageMap);
            return new TypeRef(new FunctionType(paramTypes.length, paramTypes, returnType));
        } else {
            return this;
        }
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
                } else if (!getType().equals(other.getType())) {
                    throw new UnifyException("Can't unify "+getType()+" with "+other.getType());
                }
            } else {
                other.setType(this.getType());
            }
        } else if (other.isFull()) {
            this.typeHolder.setType(other.getType());
            this.typeHolder.link(other.typeHolder);
        } else {
            this.typeHolder.link(other.typeHolder);
        }
    }
}
