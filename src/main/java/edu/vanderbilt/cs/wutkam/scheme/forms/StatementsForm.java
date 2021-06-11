package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AssignExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.LetExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.StatementsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;

import java.util.ArrayList;
import java.util.List;

/**
 */
public class StatementsForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 2) {
            throw new LispException("statements form must have at least one statement");
        }

        List<Expression> statements = new ArrayList<>();
        for (Expression expr: aList.elementsFrom(1)) {
            if (expr instanceof ListExpr) {
                ListExpr listExpr = (ListExpr) expr;
                if (listExpr.size() < 1) {
                    throw new LispException("can't have an empty list as a statement");
                }
                if (listExpr.getElement(0) instanceof SymbolExpr) {
                    if (((SymbolExpr)listExpr.getElement(0)).value.equals(":=")) {
                        statements.add(parseAssignment(listExpr.elementsFrom(1)));
                    } else {
                        statements.add(FormExpander.expand(listExpr, false));
                    }
                }
            } else {
                throw new LispException("A statement must be a special form or a function application");
            }
        }
        return new StatementsExpr(statements);
    }

    protected Expression parseAssignment(List<Expression> decl) throws LispException {
        if (decl.size() != 2) {
            throw new LispException("assignment "+decl+" should be a list with a name and expression");
        }

        // If the first element in a declaration is a symbol, create a symbol declaration
        if (decl.get(0) instanceof SymbolExpr) {
            SymbolExpr sym = (SymbolExpr) decl.get(0);
            Expression value = decl.get(1);
            if (value instanceof ListExpr) {
                value = FormExpander.expand((ListExpr) value, false);
            }
            return new AssignExpr(new LetExpr.SymbolDeclaration(sym.value, value));

            // Otherwise, if it is a list, assume it is a type constructor
        } else if (decl.get(0) instanceof ListExpr) {
            // It can't be empty
            ListExpr bindList = (ListExpr) decl.get(0);
            if (bindList.size() < 0) {
                throw new LispException("Assignment binding must have a constructor name and parameters");
            }

            // The bindind can only contain symbols
            for (Expression expr: bindList.elementsFrom(0)) {
                if (!(expr instanceof SymbolExpr)) {
                    throw new LispException("Assignment binding must contain only symbols");
                }
            }

            // Get the constructor name and find the type by the constructor name
            String constructorName = ((SymbolExpr)bindList.getElement(0)).value;
            AbstractTypeDecl abstractTypeDecl = SchemeRuntime.getTypeRegistry().findByConstructor(constructorName);
            if (abstractTypeDecl == null) {
                throw new LispException("Unknown type constructor "+constructorName);
            }

            // This only makes sense from a type-safety standpoint if the type has one constructor. Otherwise
            // you should use match to handle each possible case
            if (abstractTypeDecl.typeConstructors.size() > 1) {
                throw new LispException("Assignment binding only allowed on types with one constructor");
            }

            // Get the definition for this constructor
            TypeConstructorExpr constructor = abstractTypeDecl.typeConstructors.get(constructorName);

            // Make sure the number of parameters match (bindList includes the constructor name, so subtract 1)
            if (constructor.paramTypes.length != bindList.size()-1) {
                throw new LispException("Expected "+constructor.paramTypes.length+
                        " parameters for type constructor, but got "+(bindList.size()-1));
            }

            // Expand the value - the expression that should generate the abstract type instance
            // that the type constructor will be matched against
            Expression value = decl.get(1);
            if (value instanceof ListExpr) {
                value = FormExpander.expand((ListExpr) value, false);
            }

            String[] paramNames = new String[bindList.size()-1];
            for (int i=0; i < paramNames.length; i++) {
                paramNames[i] = ((SymbolExpr)bindList.getElement(i+1)).value;
            }

            // Create a new MatchDeclaration instance
            return new AssignExpr(new LetExpr.MatchDeclaration(abstractTypeDecl.typeName,
                    constructorName, paramNames, value));
        } else {
            throw new LispException("Assignment should either contains a symbol or a type constructor");
        }
    }
}
