package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 6/3/21
 * Time: 1:33 PM
 */
public class AbstractTypeDecl {
    public String typeName;
    public List<TypeRef> parametricTypes;
    public List<TypeConstructorExpr> typeConstructors;

    public AbstractTypeDecl(String typeName, List<TypeRef> parametricTypes,
                            List<TypeConstructorExpr> typeConstructors) {
        this.typeName = typeName;
        this.parametricTypes = parametricTypes;
        this.typeConstructors = typeConstructors;
    }
}
