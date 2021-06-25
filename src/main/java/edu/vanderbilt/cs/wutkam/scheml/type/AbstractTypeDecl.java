package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.TypeConstructorExpr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** The declaration of an abstract type. The main difference between this and AbstractType is that this
 * object also contains all the type constructors.
 */
public class AbstractTypeDecl {
    public String typeName;
    public List<TypeRef> parametricTypes;
    public Map<String,TypeConstructorExpr> typeConstructors;

    public AbstractTypeDecl(String typeName, List<TypeRef> parametricTypes) {
        this.typeName = typeName;
        this.parametricTypes = parametricTypes;
    }

    public AbstractTypeDecl(String typeName, List<TypeRef> parametricTypes,
                            Map<String,TypeConstructorExpr> typeConstructors) {
        this.typeName = typeName;
        this.parametricTypes = parametricTypes;
        this.typeConstructors = typeConstructors;
    }

    public void addTypeConstructors(Map<String,TypeConstructorExpr> typeConstructors) {
        this.typeConstructors = typeConstructors;
    }

    public AbstractTypeDecl copy() {
        // Create a linkage map so that all the named parametric types will be linked with their usage
        // in the various type constructors
        Map<String,TypeRef> linkageMap = new HashMap<>();
        List<TypeRef> newParametricTypes = new ArrayList<>();

        // Create a new list with copies of the parametric types
        for (TypeRef ref: parametricTypes) {
            newParametricTypes.add(ref.copy(linkageMap));
        }

        if (typeConstructors != null) {
            Map<String, TypeConstructorExpr> newTypeConstructors = new HashMap<>();

            // Copy each type constructor
            for (String key : typeConstructors.keySet()) {
                TypeConstructorExpr typeConstructor = typeConstructors.get(key);

                // Create copies of the parametric types in the constructor (the linkage map will make them
                // refer to the same types as those from the abstract type itself
                List<TypeRef> parametricTypes = new ArrayList<>();
                for (TypeRef ref : typeConstructor.parametricTypes) {
                    parametricTypes.add(ref.copy(linkageMap));
                }

                // Create copies of the parameter types for the type constructor, again linking them
                // with the parametric types in the abstract type and the other constructors
                List<TypeRef> paramTypes = new ArrayList<>();
                for (int i = 0; i < typeConstructor.paramTypes.length; i++) {
                    paramTypes.add(typeConstructor.paramTypes[i].copy(linkageMap));
                }

                // Add this constructor to the new constructor map
                newTypeConstructors.put(key, new TypeConstructorExpr(typeConstructor.typeName,
                        typeConstructor.name, parametricTypes, paramTypes));
            }
            // Return the new type decl
            return new AbstractTypeDecl(typeName, newParametricTypes, newTypeConstructors);
        } else {
            // If we don't have the constructors yet, just return a copy of the decl without them
            return new AbstractTypeDecl(typeName, newParametricTypes);
        }
    }
}
