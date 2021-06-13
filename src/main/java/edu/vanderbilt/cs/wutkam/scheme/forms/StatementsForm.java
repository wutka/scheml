package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.AssignExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.LetExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.StatementsExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.AbstractTypeDecl;

import java.util.ArrayList;
import java.util.List;

/** Represents a block of statements treated as a single expression, similar to (progn), but providing
 * variable binding similar to let, using the (:=) form
 * for example:
 * (statements
 *   (print "Say something: ")
 *   (:= line (input))
 *   (printf "You typed %s\n" line))
 *
 * The (:=) form allows for the same kind of bindings as let, for example:
 * (type pair ('a 'b) (Pair 'a 'b))
 * (define (do-something) (Pair "foo" 42))
 * (statements
 *   (:= (Pair name i) (do-something))
 *   (printf "Name %s has value %d\n" name i))
 */
public class StatementsForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 2) {
            throw new LispException("statements form must have at least one statement");
        }

        List<Expression> statements = new ArrayList<>();
        // Parse each statement
        for (Expression expr: aList.elementsFrom(1)) {
            if (expr instanceof ListExpr) {
                ListExpr listExpr = (ListExpr) expr;
                if (listExpr.size() < 1) {
                    throw new LispException("can't have an empty list as a statement");
                }
                if (listExpr.getElement(0) instanceof SymbolExpr) {
                    // If the expression starts with :=, parse this as an assignment
                    if (((SymbolExpr)listExpr.getElement(0)).value.equals(":=")) {
                        statements.add(parseAssignment(listExpr.elementsFrom(1)));
                    } else {
                        // Otherwise it is just a normal expression
                        statements.add(FormExpander.expand(listExpr, false));
                    }
                }
            } else {
                // A statement can just be a value, which is really only useful as the
                // last statement in the list, because earlier ones don't affect anything
                statements.add(expr);
            }
        }
        return new StatementsExpr(statements);
    }

    /** Parse a (:=) assignment as if it was a let binding */
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
