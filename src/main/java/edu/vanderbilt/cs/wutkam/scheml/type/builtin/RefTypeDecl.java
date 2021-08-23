package edu.vanderbilt.cs.wutkam.scheml.type.builtin;

import edu.vanderbilt.cs.wutkam.scheml.expr.*;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.*;

/** A built-in AbstractTypeDecl for references
 */
public class RefTypeDecl extends AbstractTypeDecl implements CustomToScheml {
    public static String refTypeName = "ref";
    static TypeRef parametricType = new TypeRef();

    public RefTypeDecl() {
        super(refTypeName, Arrays.asList(new TypeRef()));
        this.addValueConstructors(createConstructorMap(this));
    }

    protected RefTypeDecl(String typeName, List<TypeRef> parametricTypes) {
        super(typeName, parametricTypes);
    }

    protected RefTypeDecl(String typeName, List<TypeRef> parametricTypes,
                          Map<String, ValueConstructorExpr> valueConstructors) {
        super(typeName, parametricTypes, valueConstructors);
    }

    @Override
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
            return new RefTypeDecl(typeName, newParametricTypes, newValueConstructors);
        } else {
            // If we don't have the constructors yet, just return a copy of the decl without them
            return new RefTypeDecl(typeName, newParametricTypes);
        }
    }

    public Expression customToScheml(AbstractTypeExpr expr) {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("Ref"));
        scheml.add(expr.values.get(0).toScheml());
        return new ListExpr(scheml);
    }

    protected static Map<String, ValueConstructorExpr> createConstructorMap(RefTypeDecl decl) {
        TypeRef refTargetType = new TypeRef();
        ValueConstructorExpr refConstructor = new ValueConstructorExpr(refTypeName, "Ref",
                Arrays.asList(parametricType), Arrays.asList(parametricType));

        TypeRef abstractTypeRef = new TypeRef(new AbstractType("ref", Arrays.asList(refTargetType)));

        Map<String, ValueConstructorExpr> refMap = new HashMap<>();
        refMap.put("Ref", refConstructor);
        SchemlRuntime.getTopLevel().define("Ref", refConstructor);
        SchemlRuntime.getUnifyTopLevel().define("Ref",
                new TypeRef(new FunctionType(1, new TypeRef[] { refTargetType},
                        abstractTypeRef)));

        return refMap;
    }

    public static boolean isRef(Expression expr) {
        if (!(expr instanceof AbstractTypeExpr)) return false;
        return ((AbstractTypeExpr)expr).typeName.equals(refTypeName);
    }

    public static AbstractTypeExpr newRef(Expression expr) {
        return new AbstractTypeExpr(refTypeName, "Ref",
                Arrays.asList(expr));
    }
}
