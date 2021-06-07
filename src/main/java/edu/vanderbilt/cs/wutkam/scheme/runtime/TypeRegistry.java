package edu.vanderbilt.cs.wutkam.scheme.runtime;

import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashMap;
import java.util.Map;

/** A registry for abstract types
 */
public class TypeRegistry {
    Map<String, AbstractTypeDecl> typeMap;

    public TypeRegistry() {
        typeMap = new HashMap<>();
    }

    /** Locates an abstract type and returns null if it doesn't exist. If it does exist, a copy of it is
     * returned so that any changes to its type refs will not affect the copy in the global environment
     */
    public AbstractTypeDecl lookup(String name) {
        AbstractTypeDecl decl = typeMap.get(name);
        if (decl == null) return null;
        return decl.copy();
    }

    /** Stores a type definition in the registry */
    public void define(AbstractTypeDecl type) {
        typeMap.put(type.typeName, type);
    }

    /** Finds the type that a type constructor is associated with. A type constructor can only be associated
     * with a single type
     */
    public AbstractTypeDecl findByConstructor(String constructorName) {
        for (AbstractTypeDecl abstractType: typeMap.values()) {
            if (abstractType.typeConstructors.containsKey(constructorName)) {
                return abstractType.copy();
            }
        }
        return null;
    }
}
