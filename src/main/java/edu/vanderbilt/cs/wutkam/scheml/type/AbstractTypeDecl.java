package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.expr.ValueConstructorExpr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** The declaration of an abstract type. The main difference between this and AbstractType is that this
 * object also contains all the value constructors.
 */
public class AbstractTypeDecl {
    public String typeName;
    public List<TypeRef> parametricTypes;
    public Map<String, ValueConstructorExpr> valueConstructors;

    public AbstractTypeDecl(String typeName, List<TypeRef> parametricTypes) {
        this.typeName = typeName;
        this.parametricTypes = parametricTypes;
    }

    public AbstractTypeDecl(String typeName, List<TypeRef> parametricTypes,
                            Map<String, ValueConstructorExpr> valueConstructors) {
        this.typeName = typeName;
        this.parametricTypes = parametricTypes;
        this.valueConstructors = valueConstructors;
    }

    public void addValueConstructors(Map<String, ValueConstructorExpr> valueConstructors) {
        this.valueConstructors = valueConstructors;
    }

    public AbstractTypeDecl copy() {
        // Create a linkage map so that all the named parametric types will be linked with their usage
        // in the various value constructors
        Map<String,TypeRef> linkageMap = new HashMap<>();
        List<TypeRef> newParametricTypes = new ArrayList<>();

        // Create a new list with copies of the parametric types
        for (TypeRef ref: parametricTypes) {
            newParametricTypes.add(ref.copy(linkageMap));
        }

        if (valueConstructors != null) {
            Map<String, ValueConstructorExpr> newValueConstructors = new HashMap<>();

            // Copy each value constructor
            for (String key : valueConstructors.keySet()) {
                ValueConstructorExpr valueConstructor = valueConstructors.get(key);

                // Create copies of the parametric types in the constructor (the linkage map will make them
                // refer to the same types as those from the abstract type itself
                List<TypeRef> parametricTypes = new ArrayList<>();
                for (TypeRef ref : valueConstructor.parametricTypes) {
                    parametricTypes.add(ref.copy(linkageMap));
                }

                // Create copies of the parameter types for the value constructor, again linking them
                // with the parametric types in the abstract type and the other constructors
                List<TypeRef> paramTypes = new ArrayList<>();
                for (int i = 0; i < valueConstructor.paramTypes.length; i++) {
                    paramTypes.add(valueConstructor.paramTypes[i].copy(linkageMap));
                }

                // Add this constructor to the new constructor map
                newValueConstructors.put(key, new ValueConstructorExpr(valueConstructor.typeName,
                        valueConstructor.name, parametricTypes, paramTypes));
            }
            // Return the new type decl
            return new AbstractTypeDecl(typeName, newParametricTypes, newValueConstructors);
        } else {
            // If we don't have the constructors yet, just return a copy of the decl without them
            return new AbstractTypeDecl(typeName, newParametricTypes);
        }
    }
}
