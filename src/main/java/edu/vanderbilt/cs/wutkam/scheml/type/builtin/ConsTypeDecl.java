package edu.vanderbilt.cs.wutkam.scheml.type.builtin;

import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.TypeConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.FunctionType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** A built-in AbstractTypeDecl for lists (Cons)
 * including some utility methods for creating Cons and Nil instances and a
 * custom toString that renders a Cons structure as a simple list
 */
public class ConsTypeDecl extends AbstractTypeDecl implements CustomToString {
    public static String consTypeName = "cons";
    static TypeRef parametricType = new TypeRef();

    public ConsTypeDecl() {
        super(consTypeName, Arrays.asList(new TypeRef()));
        this.addTypeConstructors(createConstructorMap(this));
    }

    protected ConsTypeDecl(String typeName, List<TypeRef> parametricTypes) {
        super(typeName, parametricTypes);
    }

    protected ConsTypeDecl(String typeName, List<TypeRef> parametricTypes,
                           Map<String, TypeConstructorExpr> typeConstructors) {
        super(typeName, parametricTypes, typeConstructors);
    }

    @Override
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
            return new ConsTypeDecl(typeName, newParametricTypes, newTypeConstructors);
        } else {
            // If we don't have the constructors yet, just return a copy of the decl without them
            return new ConsTypeDecl(typeName, newParametricTypes);
        }
    }

    /** Renders this abstract type as a simple list */
    public String customToString(AbstractTypeExpr expr) {
        StringBuilder builder = new StringBuilder();
        if (expr.constructorName.equals("Nil")) {
            return "Nil";
        }
        AbstractTypeExpr curr = expr;
        builder.append('(');
        boolean first = true;
        while (curr.constructorName.equals("Cons")) {
            if (!first) builder.append(" ");
            first = false;
            builder.append(curr.values.get(0).toString());
            curr = (AbstractTypeExpr) curr.values.get(1);
        }
        builder.append(')');
        return builder.toString();
    }

    protected static Map<String, TypeConstructorExpr> createConstructorMap(ConsTypeDecl decl) {
        TypeConstructorExpr nilConstructor = new TypeConstructorExpr("cons", "Nil",
                Arrays.asList(parametricType), new ArrayList<>());
        TypeConstructorExpr consConstructor = new TypeConstructorExpr("cons", "Cons",
                Arrays.asList(parametricType), Arrays.asList(parametricType,
                new TypeRef(new AbstractType(decl))));

        TypeRef consTargetType = new TypeRef();
        TypeRef abstractTypeRef = new TypeRef(new AbstractType("cons", Arrays.asList(consTargetType)));

        Map<String,TypeConstructorExpr> consMap = new HashMap<>();
        consMap.put("Nil", nilConstructor);
        SchemlRuntime.getTopLevel().define("Nil", nilConstructor);
        SchemlRuntime.getUnifyTopLevel().define("Nil",
                new TypeRef(new FunctionType(0, new TypeRef[0], abstractTypeRef)));
        consMap.put("Cons", consConstructor);
        SchemlRuntime.getTopLevel().define("Cons", consConstructor);
        SchemlRuntime.getUnifyTopLevel().define("Cons",
                new TypeRef(new FunctionType(2, new TypeRef[] { consTargetType, abstractTypeRef},
                        abstractTypeRef)));

        return consMap;
    }

    public static AbstractTypeExpr newNil() {
        return new AbstractTypeExpr(consTypeName, "Nil", new ArrayList<>());
    }

    public static AbstractTypeExpr newCons(Expression head, Expression tail) {
        return new AbstractTypeExpr(consTypeName, "Cons",
                Arrays.asList(head, tail));
    }

    public static AbstractType newConsType(TypeRef elementType) {
        return new AbstractType(consTypeName, Arrays.asList(elementType));
    }
}