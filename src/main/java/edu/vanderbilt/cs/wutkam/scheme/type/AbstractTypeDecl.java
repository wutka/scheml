package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 6/3/21
 * Time: 1:33 PM
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
        Map<String,TypeRef> linkageMap = new HashMap<>();
        List<TypeRef> newParametricTypes = new ArrayList<>();
        for (TypeRef ref: parametricTypes) {
            newParametricTypes.add(ref.copy(linkageMap));
        }
        if (typeConstructors != null) {
            Map<String, TypeConstructorExpr> newTypeConstructors = new HashMap<>();
            for (String key : typeConstructors.keySet()) {
                TypeConstructorExpr typeConstructor = typeConstructors.get(key);
                List<TypeRef> parametricTypes = new ArrayList<>();
                for (TypeRef ref : typeConstructor.parametricTypes) {
                    parametricTypes.add(ref.copy(linkageMap));
                }
                List<TypeRef> paramTypes = new ArrayList<>();
                for (int i = 0; i < typeConstructor.paramTypes.length; i++) {
                    paramTypes.add(typeConstructor.paramTypes[i].copy(linkageMap));
                }
                newTypeConstructors.put(key, new TypeConstructorExpr(typeConstructor.typeName,
                        typeConstructor.name, parametricTypes, paramTypes));
            }
            return new AbstractTypeDecl(typeName, newParametricTypes, newTypeConstructors);
        } else {
            return new AbstractTypeDecl(typeName, newParametricTypes);
        }
    }
}
