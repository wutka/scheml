package edu.vanderbilt.cs.wutkam.scheme.runtime;

import edu.vanderbilt.cs.wutkam.scheme.type.AbstractType;

import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 6/4/21
 * Time: 4:47 PM
 */
public class TypeRegistry {
    Map<String,AbstractType> typeMap;

    public TypeRegistry() {
        typeMap = new HashMap<>();
    }

    public AbstractType lookup(String name) {
        return typeMap.get(name);
    }

    public void define(AbstractType type) {
        typeMap.put(type.typeName, type);
    }

    public AbstractType findByConstructor(String constructorName) {
        for (AbstractType abstractType: typeMap.values()) {
            if (abstractType.typeConstructors.containsKey(constructorName)) {
                return abstractType;
            }
        }
        return null;
    }
}
