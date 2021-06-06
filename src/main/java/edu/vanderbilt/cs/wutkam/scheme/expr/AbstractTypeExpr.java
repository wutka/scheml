package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.List;

public class AbstractTypeExpr implements Expression {
    public String typeName;
    public String constructorName;
    public List<Expression> values;

    public AbstractTypeExpr(String typeName, String constructorName, List<Expression> values) {
        this.typeName = typeName;
        this.constructorName = constructorName;
        this.values = values;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        AbstractTypeDecl typeDecl = SchemeRuntime.getTypeRegistry().lookup(typeName);
        TypeConstructorExpr typeConstructor = typeDecl.typeConstructors.get(constructorName);
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
