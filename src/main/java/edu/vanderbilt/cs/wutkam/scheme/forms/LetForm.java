package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.*;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/26/21
 * Time: 3:30 PM
 */
public class LetForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 3) {
            throw new LispException("let form requires declarations and a body");
        }

        int letType = LetExpr.LET_FORM;
        if (aList.getElement(0) instanceof SymbolExpr) {
            SymbolExpr sym = (SymbolExpr) aList.getElement(0);
            if (sym.value.equals("let*")) {
                letType = LetExpr.LET_STAR_FORM;
            } else if (sym.value.equals("letrec")) {
                letType = LetExpr.LET_REC_FORM;
            }
        }
        if (!(aList.getElement(1) instanceof ListExpr)) {
            throw new LispException("let form requires a list of declarations");
        }
        ListExpr decls = (ListExpr) aList.getElement(1);
        List<LetExpr.Declaration> letDeclarations = new ArrayList<>();

        for (Expression declExpr: decls.elements) {
            if (!(declExpr instanceof ListExpr)) {
                throw new LispException("declaration "+declExpr+" in let should be a list with a name and expression");
            }
            ListExpr decl = (ListExpr) declExpr;
            if (decl.size() != 2) {
                throw new LispException("declaration "+declExpr+" in let should be a list with a name and expression");
            }

            if (decl.getElement(0) instanceof SymbolExpr) {
                SymbolExpr sym = (SymbolExpr) decl.getElement(0);
                Expression value = decl.elements.get(1);
                if (value instanceof ListExpr) {
                    value = FormExpander.expand((ListExpr) value, false);
                }
                letDeclarations.add(new LetExpr.SymbolDeclaration(sym.value, value));
            } else if (decl.getElement(0) instanceof ListExpr) {
                ListExpr bindList = (ListExpr) decl.getElement(0);
                if (bindList.size() < 0) {
                    throw new LispException("Let match binding must have a constructor name and parameters");
                }
                for (Expression expr: bindList.elementsFrom(0)) {
                    if (!(expr instanceof SymbolExpr)) {
                        throw new LispException("Let match binding must contain only symbols");
                    }
                }
                String constructorName = ((SymbolExpr)bindList.getElement(0)).value;
                AbstractTypeDecl abstractTypeDecl = SchemeRuntime.getTypeRegistry().findByConstructor(constructorName);
                if (abstractTypeDecl == null) {
                    throw new LispException("Unknown type constructor "+constructorName);
                }

                if (abstractTypeDecl.typeConstructors.size() > 1) {
                    throw new LispException("Let match binding only allowed on types with one constructor");
                }

                TypeConstructorExpr constructor = abstractTypeDecl.typeConstructors.get(constructorName);

                if (constructor.paramTypes.length != bindList.size()-1) {
                    throw new LispException("Expected "+constructor.paramTypes.length+
                            " parameters for type constructor, but got "+(bindList.size()-1));
                }

                Expression value = decl.elements.get(1);
                if (value instanceof ListExpr) {
                    value = FormExpander.expand((ListExpr) value, false);
                }
                String[] paramNames = new String[bindList.size()-1];
                for (int i=0; i < paramNames.length; i++) {
                    paramNames[i] = ((SymbolExpr)bindList.getElement(i+1)).value;
                }
                letDeclarations.add(new LetExpr.MatchDeclaration(abstractTypeDecl.typeName,
                        constructorName, paramNames, value));
            } else {
                throw new LispException("Let binding should either contains a symbol or a type constructor");
            }
        }

        List<Expression> body = new ArrayList<>();
        for (Expression expr: aList.elementsFrom(2)) {
            if (expr instanceof ListExpr) {
                expr = FormExpander.expand((ListExpr) expr, false);
            }
            body.add(expr);
        }
         
        return new LetExpr(letDeclarations, letType, body);
    }
}
