package edu.vanderbilt.cs.wutkam.scheml.runtime;

import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.HashMap;
import java.util.Map;

/** A registry for abstract types
 */
public class TypeRegistry {
    Map<String, AbstractTypeDecl> typeMap;

    public TypeRegistry() {
        typeMap = new HashMap<>();
        initializeBuiltinTypes(typeMap);
    }

    protected void initializeBuiltinTypes(Map<String, AbstractTypeDecl> typeMap) {
        typeMap.put("cons", new ConsTypeDecl());
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

    /** Finds the type that a value constructor is associated with. A value constructor can only be associated
     * with a single type
     */
    public AbstractTypeDecl findByConstructor(String constructorName) {
        for (AbstractTypeDecl abstractType: typeMap.values()) {
            if (abstractType.valueConstructors.containsKey(constructorName)) {
                return abstractType.copy();
            }
        }
        return null;
    }
}
