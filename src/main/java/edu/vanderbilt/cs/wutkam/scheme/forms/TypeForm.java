package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TypeForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 3) {
            throw new LispException("Type declaration must at least have a type name and a constructor name");
        }

        List<String> parametricTypes = new ArrayList<>();
        String typeName;
        List<Expression> typeConstructors = new ArrayList<>();

        int nextPos = 1;

        Expression typeNameExpr = aList.getElement(nextPos++);
        if (typeNameExpr instanceof SymbolExpr) {
            typeName = ((SymbolExpr)typeNameExpr).value;
        } else {
            throw new LispException("Type name must be a symbol");
        }

        AbstractType abstractType;


        Expression nextExpr = aList.getElement(nextPos);
        if (nextExpr instanceof ListExpr) {
            boolean isParametric = false;
            for (Expression expr: ((ListExpr)nextExpr).elementsFrom(0)) {
                if (expr instanceof SymbolExpr) {
                    SymbolExpr sym = (SymbolExpr) expr;
                    if (sym.value.startsWith("'")) {
                        isParametric = true;
                        parametricTypes.add(sym.value);
                        // Only increment nextPos if this is a list of parametric values
                        nextPos++;
                    } else {
                        if (isParametric) {
                            throw new LispException("List of parametric types must all start with '");
                        }
                        break;
                    }
                } else {
                    if (isParametric) {
                        throw new LispException("List of parametric types must be symbols that all start with '");
                    }
                    break;
                }
            }
        }

        Map<String, TypeRef> parametricMap = new HashMap<>();
        List<TypeRef> parametricList = new ArrayList<>();
        for (String parametric: parametricTypes) {
            TypeRef typeRef = new TypeRef();
            parametricMap.put(parametric, typeRef);
            parametricList.add(typeRef);

        }

        abstractType = new AbstractType(typeName, parametricList);

        for (Expression expr: aList.elementsFrom(nextPos)) {
            if (expr instanceof SymbolExpr) {
                SymbolExpr sym = (SymbolExpr) expr;
                if (!Character.isUpperCase(sym.value.charAt(0))) {
                    throw new LispException("Type constructors should start with an upper-case letter");
                }
                typeConstructors.add(new TypeConstructorExpr(abstractType, sym.value, new ArrayList<>()));
            } else if (expr instanceof ListExpr) {
                
                
            }
        }

        // There's nothing for this form to return
        return new VoidExpr();
    }
}
