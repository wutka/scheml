package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.List;

public class TypeConstructorExpr extends FunctionExpr {
    public String typeName;
    public List<TypeRef> parametricTypes;

    public TypeConstructorExpr(String typeName, String name, List<TypeRef> parametricTypes, List<TypeRef> parameters) {
        super(name, parameters, new TypeRef(new AbstractType(typeName, parametricTypes)));
        this.typeName = typeName;
        this.parametricTypes = parametricTypes;
    }

    @Override
    public Expression apply(List<Expression> arguments, Environment<Expression> env) throws LispException {
        return new AbstractTypeExpr(typeName, this.name, arguments);
    }
}
