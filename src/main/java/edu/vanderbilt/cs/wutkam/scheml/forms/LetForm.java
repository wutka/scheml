package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.*;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;

import java.util.ArrayList;
import java.util.List;

/** Expands the form (let ((var1 expr1) (var2 expr2) ...) body) into a LetExpr
 * The var part of the declaration can either be a symbol or a type constructor for an abstract data type
 * that only has one constructor (you must use the match form when an abstract type has multiple constructors).
 * For instance:
 * (type point-3d (Point double double double))
 * (let (((Point x y z) (find-center universe)))
 *    (printf "The center of the universe is at %f,%f,%f\n" x y z))
 *
 * Note that an extra pair of parens is needed when specifying a type constructor since it is a list instead
 * of a symbol.
 *
 * In addition to let, it expands let* and letrec. The let* form allows subsequent let declarations (bindings)
 * to see the ones that came before it, and the letrec form allows a declaration to see itself so that a
 * lambda defined in a let binding can call itself recursively. The letrec binding can be useful in defining
 * tail recursive functions where the tail-recursive implementation requires some extra parameters, so a main
 * function is defined that contains the tail-recursive part defined in a letrec:
 *
 * For example, a tail-recursive version of the factorial function uses an accumulator parameter to contain
 * the numbers multiplied together so far. The main function just takes n, and then invokes the fact-1
 * tail-recursive implementation with the additional accumulator parameter initialized to 1:
 * (define (fact n)
 *   (letrec ((fact-1 (lambda (n acc)
 *                      (if (< n 2) acc
 *                          (fact-1 (- n 1) (* acc n))))))
 *     (fact-1 n 1)))
 */
public class LetForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 3) {
            throw new LispException("let form requires declarations and a body");
        }

        // Assume the let form by default
        int letType = LetExpr.LET_FORM;

        // Check to see if it is a different one
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

        for (Expression declExpr: decls.elementsFrom(0)) {
            if (!(declExpr instanceof ListExpr)) {
                throw new LispException("declaration "+declExpr+" in let should be a list with a name and expression");
            }
            ListExpr decl = (ListExpr) declExpr;
            if (decl.size() != 2) {
                throw new LispException("declaration "+declExpr+" in let should be a list with a name and expression");
            }

            // If the first element in a declaration is a symbol, create a symbol declaration
            if (decl.getElement(0) instanceof SymbolExpr) {
                SymbolExpr sym = (SymbolExpr) decl.getElement(0);
                Expression value = decl.getElement(1);
                if (value instanceof ListExpr) {
                    value = FormExpander.expand((ListExpr) value, false);
                }
                letDeclarations.add(new LetExpr.SymbolDeclaration(sym.value, value));

            // Otherwise, if it is a list, assume it is a type constructor
            } else if (decl.getElement(0) instanceof ListExpr) {
                // It can't be empty
                ListExpr bindList = (ListExpr) decl.getElement(0);
                if (bindList.size() < 0) {
                    throw new LispException("Let match binding must have a constructor name and parameters");
                }

                // The bindind can only contain symbols
                for (Expression expr: bindList.elementsFrom(0)) {
                    if (!(expr instanceof SymbolExpr)) {
                        throw new LispException("Let match binding must contain only symbols");
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
                    throw new LispException("Let match binding only allowed on types with one constructor");
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
                Expression value = decl.getElement(1);
                if (value instanceof ListExpr) {
                    value = FormExpander.expand((ListExpr) value, false);
                }

                String[] paramNames = new String[bindList.size()-1];
                for (int i=0; i < paramNames.length; i++) {
                    paramNames[i] = ((SymbolExpr)bindList.getElement(i+1)).value;
                }

                // Create a new MatchDeclaration instance
                letDeclarations.add(new LetExpr.MatchDeclaration(abstractTypeDecl.typeName,
                        constructorName, paramNames, value));
            } else {
                throw new LispException("Let binding should either contains a symbol or a type constructor");
            }
        }

        // Expand each expression in the let body if necessary
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
