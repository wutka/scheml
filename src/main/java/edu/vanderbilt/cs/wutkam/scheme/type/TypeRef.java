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
        } else {
            // Try copying the type
            Type copy = getType().copy(linkageMap);

            // If the type is the same as the current one (i.e. it's a simple type), don't copy this
            if (copy == getType()) return this;

            // Otherwise return a new typeref
            return new TypeRef(copy);
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
                getType().unify(other.getType());
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
