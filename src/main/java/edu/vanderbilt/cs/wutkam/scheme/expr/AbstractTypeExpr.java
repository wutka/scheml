package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;
import edu.vanderbilt.cs.wutkam.scheme.type.builtin.CustomToString;

import java.util.List;

/** Represents an instance of an abstract type as created from a type constructor function */
public class AbstractTypeExpr implements Expression {

    /** The name of the abstract type this is an instance of */
    public String typeName;

    /** The name of the type constructor that constructed this instance */
    public String constructorName;

    /** The values passed to the type constructor when this instance was created. These values
     * should already have been evaluated when the type constructor function was evaluated, so
     * there is no need to override the evaluate function in this class.
     */
    public List<Expression> values;

    public AbstractTypeExpr(String typeName, String constructorName, List<Expression> values) {
        this.typeName = typeName;
        this.constructorName = constructorName;
        this.values = values;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        AbstractTypeDecl typeDecl = SchemlRuntime.getTypeRegistry().lookup(typeName);
        TypeConstructorExpr typeConstructor = typeDecl.typeConstructors.get(constructorName);
        // We unify with the values here to make sure the return type has concrete values
        // when necessary
        for (int i=0; i < typeConstructor.paramTypes.length; i++) {
            values.get(i).unify(typeConstructor.paramTypes[i], env);
        }
        typeRef.unify(typeConstructor.returnType);
    }

    @Override
    public boolean equals(Object otherObj) {
        if (!(otherObj instanceof AbstractTypeExpr)) return false;

        AbstractTypeExpr other = (AbstractTypeExpr) otherObj;
        if (!typeName.equals(other.typeName)) return false;

        if (!(constructorName.equals(other.constructorName))) return false;

        if (values.size() != other.values.size()) return false;

        for (int i=0; i < values.size(); i++) {
            if (!values.get(i).equals(other.values.get(i))) return false;
        }
        return true;
    }

    @Override
    public String toString() {
        AbstractTypeDecl decl = SchemlRuntime.getTypeRegistry().lookup(this.typeName);
        if (decl instanceof CustomToString) {
            return ((CustomToString)decl).customToString(this);
        }
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        builder.append(constructorName);
        for (Expression value: values) {
            builder.append(" ");
            builder.append(value.toString());
        }
        builder.append(")");
        return builder.toString();
    }
}
