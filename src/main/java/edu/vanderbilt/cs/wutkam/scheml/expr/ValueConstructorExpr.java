package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.List;

/** Represents a value constructor function that creates a new instance of a type */
public class ValueConstructorExpr extends FunctionExpr {
    public String typeName;
    public List<TypeRef> parametricTypes;

    public ValueConstructorExpr(String typeName, String name, List<TypeRef> parametricTypes, List<TypeRef> parameters) {
        super(name, parameters, new TypeRef(new AbstractType(typeName, parametricTypes)));
        this.typeName = typeName;
        this.parametricTypes = parametricTypes;
    }

    @Override
    public Expression apply(List<Expression> arguments, Environment<Expression> env) throws LispException {
        // You can partially apply a value constructor func too
        if (arguments.size() < paramTypes.length) {
            return new PartialApplicationExpr(this, arguments);
        } else {
            return new AbstractTypeExpr(typeName, this.name, arguments);
        }
    }
}
