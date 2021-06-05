package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class TypeRef {
    Type type;
    Set<TypeRef> linked;

    public TypeRef() {
        type = new EmptyType();
        linked = new HashSet<>();
        linked.add(this);
    }

    public TypeRef(Type type) {
        this.type = type;
        linked = new HashSet<>();
        linked.add(this);
    }

    public boolean isFull() {
        return !(type instanceof EmptyType);
    }

    public TypeRef copy(Map<String,TypeRef> linkageMap) {
        if (type instanceof EmptyType) {
            EmptyType thisType = (EmptyType) type;
            TypeRef newCopy = linkageMap.get(thisType.id);
            if (newCopy == null) {
                newCopy = new TypeRef();
                linkageMap.put(thisType.id, newCopy);
            }
            return newCopy;
        }
        else if (type instanceof ConsType) {
            ConsType thisType = (ConsType) type;
            return new TypeRef(new ConsType(thisType.elementType.copy(linkageMap)));
        } else if (type instanceof FunctionType) {
            FunctionType thisType = (FunctionType) type;
            TypeRef[] paramTypes = new TypeRef[thisType.arity];
            for (int i = 0; i < paramTypes.length; i++) {
                paramTypes[i] = thisType.paramTypes[i].copy(linkageMap);
            }
            TypeRef returnType = thisType.returnType.copy(linkageMap);
            return new TypeRef(new FunctionType(paramTypes.length, paramTypes, returnType));
        } else if (type instanceof AbstractType) {
            AbstractType thisType = (AbstractType) type;
            List<TypeRef> typeParameters = new ArrayList<>();
            for (TypeRef typeRef: thisType.typeParameters) {
                typeParameters.add(typeRef.copy(linkageMap));
            }
            Map<String,TypeConstructorExpr> typeConstructors = new HashMap<>();
            for (String constructorName: thisType.typeConstructors.keySet()) {
                TypeConstructorExpr funcType = thisType.typeConstructors.get(constructorName);
                TypeRef ref = new TypeRef(funcType);
                typeConstructors.put(constructorName, (FunctionType) ref.copy(linkageMap).getType());
            }
            return new TypeRef(new AbstractType(thisType.typeName, typeParameters, typeConstructors));
        } else {
            return this;
        }
    }

    public Type getType() {
        return type;
    }

    public void setType(Type other) throws LispException {
        if (this.type instanceof EmptyType) {
            for (TypeRef link : linked) {
                link.type = other;
            }
        }
    }

    public void link(TypeRef other) throws LispException {
        linked.addAll(other.linked);
        for (TypeRef otherRef: linked) {
            otherRef.linked.addAll(linked);
        }
        if (!linked.equals(other.linked)) {
            throw new RuntimeException("linked TypeRef mismatch");
        }
        setType(this.type);
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

                    for (int i = 0; i < thisFunc.arity; i++) {
                        try {
                            thisFunc.paramTypes[i].unify(otherFunc.paramTypes[i]);
                        } catch (UnifyException exc) {
                            throw UnifyException.addCause("Can't unify parameter " + i + " of " + getType() + " with " +
                                    other.getType(), exc);
                        }
                    }

                    try {
                        thisFunc.returnType.unify(otherFunc.returnType);
                    } catch (UnifyException exc) {
                        throw UnifyException.addCause("Can't unify return type of " + getType() +
                                " with " + other.getType(), exc);
                    }
                } else if (getType() instanceof AbstractType) {
                    if (!(other.getType() instanceof FunctionType)) {
                        throw new UnifyException("Unable to unify " + getType() + " with " + other.getType());
                    }
                    AbstractType thisType = (AbstractType) getType();
                    AbstractType otherType = (AbstractType) other.getType();
                    if (!thisType.typeName.equals(otherType.typeName)) {
                        throw new UnifyException("Unable to unify " + getType() + " with " + other.getType());
                    }
                    if (thisType.typeParameters.size() != otherType.typeParameters.size()) {
                        throw new UnifyException("Unable to unify " + getType() + " with " + other.getType());
                    }
                    if (thisType.typeConstructors.size() != otherType.typeConstructors.size()) {
                        throw new UnifyException("Unable to unify " + getType() + " with " + other.getType() +
                                " because type constructors don't match");
                    }
                    for (int i=0; i < thisType.typeParameters.size(); i++) {
                        try {
                            thisType.typeParameters.get(i).unify(otherType.typeParameters.get(i));
                        } catch (UnifyException exc) {
                            throw UnifyException.addCause("Unable to unify " + getType() + " with " +
                                    other.getType() + " because type parameters don't match", exc);
                        }
                    }

                    for (String constructorName: thisType.typeConstructors.keySet()) {
                        TypeConstructorExpr thisConstructor = thisType.typeConstructors.get(constructorName);
                        TypeConstructorExpr otherConstructor = otherType.typeConstructors.get(constructorName);
                        if (otherConstructor == null) {
                            throw new UnifyException("Unable to unify " + getType() + " with " + other.getType() +
                                    " because type " + other.getType() + " is missing type constructor "+
                                    constructorName);
                        }
                        try {
                            (new TypeRef(thisConstructor)).unify(new TypeRef(otherConstructor));
                        } catch (UnifyException exc) {
                            throw UnifyException.addCause("Unable to unify type constructor "+
                                    constructorName+" of "+thisType+" with type constructor in "+
                                    otherType, exc);

                        }
                    }
                } else if (!getType().equals(other.getType())) {
                    throw new UnifyException("Can't unify "+getType()+" with "+other.getType());
                }
            } else {
                other.setType(this.getType());
            }
        } else if (other.isFull()) {
            this.setType(other.getType());
            this.link(other);
        } else {
            this.link(other);
        }
    }
}
