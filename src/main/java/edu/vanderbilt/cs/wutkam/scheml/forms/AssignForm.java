package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.*;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;

public class AssignForm implements Form {
    @Override
    public Expression expandForm(ListExpr decl, boolean isTopLevel) throws LispException {
        if (decl.size() != 3) {
            throw new LispException("assignment "+decl+" should be a list with a name and expression");
        }

        // If the first element in a declaration is a symbol, create a symbol declaration
        if (decl.getElement(1) instanceof SymbolExpr) {
            SymbolExpr sym = (SymbolExpr) decl.getElement(1);
            Expression value = decl.getElement(2);
            if (value instanceof ListExpr) {
                value = FormExpander.expand((ListExpr) value, false);
            }
            return new AssignExpr(new LetExpr.SymbolDeclaration(sym.value, value));

            // Otherwise, if it is a list, assume it is a type constructor
        } else if (decl.getElement(1) instanceof ListExpr) {
            // It can't be empty
            ListExpr bindList = (ListExpr) decl.getElement(1);
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
            AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().findByConstructor(constructorName);
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
            Expression value = decl.getElement(2);
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
