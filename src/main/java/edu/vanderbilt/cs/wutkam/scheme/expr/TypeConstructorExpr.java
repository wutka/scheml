package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.List;

public class TypeConstructorExpr extends FunctionExpr {
    public AbstractType abstractType;
    public String name;
    public List<TypeRef> parameters;

    public TypeConstructorExpr(AbstractType abstractType, String name, List<TypeRef> parameters) {
        super(parameters, new TypeRef(abstractType));
        this.name = name;
        this.abstractType = abstractType;
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        
    }

    @Override
    public Expression apply(List<Expression> arguments, Environment<Expression> env) throws LispException {
        return new AbstractTypeExpr(abstractType.typeName, this.name, arguments);
    }
}
