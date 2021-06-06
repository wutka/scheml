package edu.vanderbilt.cs.wutkam.scheme.runtime;

import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 6/4/21
 * Time: 4:47 PM
 */
public class TypeRegistry {
    Map<String, AbstractTypeDecl> typeMap;

    public TypeRegistry() {
        typeMap = new HashMap<>();
    }

    public AbstractTypeDecl lookup(String name) {
        return typeMap.get(name).copy();
    }

    public void define(AbstractTypeDecl type) {
        typeMap.put(type.typeName, type);
    }

    public AbstractTypeDecl findByConstructor(String constructorName) {
        for (AbstractTypeDecl abstractType: typeMap.values()) {
            if (abstractType.typeConstructors.containsKey(constructorName)) {
                return abstractType.copy();
            }
        }
        return null;
    }
}
