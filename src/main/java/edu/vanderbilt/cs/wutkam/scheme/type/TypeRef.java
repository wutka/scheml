package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/** Contains a reference to a type that can be linked to other type references that are known
 * to refer to the same type. When the type ref is unified with a concrete type, all the type refs
 * that it is linked to will all be unified with that concrete type.
 */
public class TypeRef {
    // The type this ref currently refers to
    Type type;

    // The other type refs this type ref is linked with
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

    /** A full type is one that has an actual type like BoolType or ConsType, and not EmptyType */
    public boolean isFull() {
        return !(type instanceof EmptyType);
    }

    /** Makes a copy of this type ref, using linkageMake to ensure that references to the same type are preserved */
    public TypeRef copy(Map<String,TypeRef> linkageMap) {
        if (type instanceof EmptyType) {
            // If this type is empty, look it up in the linkage map
            EmptyType thisType = (EmptyType) type;
            TypeRef newCopy = linkageMap.get(thisType.id);
            if (newCopy == null) {
                // If it is null, create a new type ref and store this type id in the linkage map
                newCopy = new TypeRef();
                linkageMap.put(thisType.id, newCopy);
            }
            return newCopy;
        } else if (type instanceof FunctionType) {
            // Copy all the param types and return types of this function
            FunctionType thisType = (FunctionType) type;
            TypeRef[] paramTypes = new TypeRef[thisType.arity];
            for (int i = 0; i < paramTypes.length; i++) {
                paramTypes[i] = thisType.paramTypes[i].copy(linkageMap);
            }
            TypeRef returnType = thisType.returnType.copy(linkageMap);
            // Return a reference to a new function type instance that uses the new copies
            return new TypeRef(new FunctionType(paramTypes.length, paramTypes, returnType));
        } else if (type instanceof AbstractType) {
            AbstractType thisType = (AbstractType) type;
            List<TypeRef> typeParameters = new ArrayList<>();
            // Copy all the abstract type's type parameters
            for (TypeRef typeRef: thisType.typeParameters) {
                typeParameters.add(typeRef.copy(linkageMap));
            }
            // Return a ref to the new copy of the abstract type
            return new TypeRef(new AbstractType(thisType.typeName, typeParameters));
        } else {
            // Simple types like BoolType and DoubleType don't need their references copied
            return this;
        }
    }

    /** Retrieves the type this reference points to */
    public Type getType() {
        return type;
    }

    /** Sets the type this reference points to (only of this reference currently points to an empty type */
    public void setType(Type other) throws LispException {
        if (this.type instanceof EmptyType) {
            // Update the types of all the type refs linked to this one
            for (TypeRef link : linked) {
                link.type = other;
            }
        }
    }

    /** Links this type to another type */
    public void link(TypeRef other) throws LispException {
        // Add references to all the links that the
        linked.addAll(other.linked);

        // For all the type refs that the other type links to, add all of this ref's links to them
        for (TypeRef otherRef: linked) {
            otherRef.linked.addAll(linked);
        }
        // Make sure all the linked types have the same type
        setType(this.type);
    }

    public void unify(TypeRef other) throws LispException {
        if (isFull()) {
            if (other.isFull()) {
                // If both types are full, they might still be complex types whose underlying components
                // are empty
                if (getType() instanceof FunctionType) {
                    // Make sure they are both functions
                    if (!(other.getType() instanceof FunctionType)) {
                        throw new UnifyException("Unable to unify " + getType() + " with " + other.getType());
                    }
                    FunctionType thisFunc = (FunctionType) getType();
                    FunctionType otherFunc = (FunctionType) other.getType();

                    // Make sure the arity matches
                    if (thisFunc.arity != otherFunc.arity) {
                        throw new UnifyException("Can't unify function " + getType() +
                                " with different arity function " + other.getType());
                    }

                    for (int i = 0; i < thisFunc.arity; i++) {
                        try {
                            // Unify the param types
                            thisFunc.paramTypes[i].unify(otherFunc.paramTypes[i]);
                        } catch (UnifyException exc) {
                            throw UnifyException.addCause("Can't unify parameter " + i + " of " + getType() + " with " +
                                    other.getType(), exc);
                        }
                    }

                    // Unify the return types
                    try {
                        thisFunc.returnType.unify(otherFunc.returnType);
                    } catch (UnifyException exc) {
                        throw UnifyException.addCause("Can't unify return type of " + getType() +
                                " with " + other.getType(), exc);
                    }
                } else if (getType() instanceof AbstractType) {
                    // Make sure they are both abstract types
                    if (!(other.getType() instanceof AbstractType)) {
                        throw new UnifyException("Unable to unify " + getType() + " with " + other.getType());
                    }
                    AbstractType thisType = (AbstractType) getType();
                    AbstractType otherType = (AbstractType) other.getType();

                    // with the same type name
                    if (!thisType.typeName.equals(otherType.typeName)) {
                        throw new UnifyException("Unable to unify " + getType() + " with " + other.getType());
                    }

                    // and same number of parametric types
                    if (thisType.typeParameters.size() != otherType.typeParameters.size()) {
                        throw new UnifyException("Unable to unify " + getType() + " with " + other.getType());
                    }

                    // unify the parametric types
                    for (int i=0; i < thisType.typeParameters.size(); i++) {
                        try {
                            thisType.typeParameters.get(i).unify(otherType.typeParameters.get(i));
                        } catch (UnifyException exc) {
                            throw UnifyException.addCause("Unable to unify " + getType() + " with " +
                                    other.getType() + " because type parameters don't match", exc);
                        }
                    }
                } else if (!getType().equals(other.getType())) {
                    // otherwise if this type is different from the other type, the unification has failed
                    throw new UnifyException("Can't unify "+getType()+" with "+other.getType());
                }
            } else {
                other.setType(this.getType());
            }
        } else if (other.isFull()) {
            // If this is empty but the other is full, set this type and link it to the other
            this.setType(other.getType());
            this.link(other);
        } else {
            // this type and the other are both empty, link them so when they are filled they will get filled
            // with the same type
            this.link(other);
        }
    }
}
