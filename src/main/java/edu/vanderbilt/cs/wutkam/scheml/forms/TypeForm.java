package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.ValueConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.*;

import java.util.*;

/** Defines an abstract type of the form (type type-name (parametric types) (Constructor-1 args) ...)
 * For example:
 * (type tree-node ('a)
 *       EmptyNode
 *       (TreeNode 'a (tree-node 'a) (tree-node 'a)))
 *
 * The list of parametric types is only necessary if the type has parametric types. If it does use them,
 * then each parametric type must be listed in the list immediately after the type name. That is,
 * if the list after the type name has ('a 'b) but then a value constructor has (Thing 'c), it is an error.
 *
 * Value constructor names must begin with an upper-case letter
 */
public class TypeForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 3) {
            throw new LispException("Type declaration must at least have a type name and a constructor name");
        }

        List<String> parametricTypes = new ArrayList<>();
        String typeName;
        List<ValueConstructorExpr> valueConstructors = new ArrayList<>();

        int nextPos = 1;

        Expression typeNameExpr = aList.getElement(nextPos++);
        if (typeNameExpr instanceof SymbolExpr) {
            typeName = ((SymbolExpr)typeNameExpr).value;
        } else {
            throw new LispException("Type name must be a symbol");
        }

        AbstractTypeDecl abstractTypeDecl;

        // Look at the expression just after the type name
        Expression nextExpr = aList.getElement(nextPos);

        // If it is a list, it might be the list of parametric types, or it might be a value constructor
        if (nextExpr instanceof ListExpr) {
            boolean isParametric = false;
            for (Expression expr: ((ListExpr)nextExpr).elementsFrom(0)) {
                // If it's a symbol that starts with ', then it is a parametric type and this is the list
                // of parametric types
                if (expr instanceof SymbolExpr) {
                    SymbolExpr sym = (SymbolExpr) expr;
                    if (sym.value.startsWith("'")) {
                        isParametric = true;
                        parametricTypes.add(sym.value);
                    } else {
                        // If we have already encountered a parametric type and we encounter a symbol
                        // that doesn't start with ', that is an error
                        if (isParametric) {
                            throw new LispException("List of parametric types must all start with '");
                        }

                        // Otherwise, if we immediately encounter a non-parametric symbol, this
                        // must be a value constructor
                        break;
                    }
                } else {
                    // If we found parametric types, but then encounter a non-symbol, that's an error
                    if (isParametric) {
                        throw new LispException("List of parametric types must be symbols that all start with '");
                    }
                    break;
                }
            }
            // Only increment nextPos if this is a list of parametric values, so that either way, nextPos
            // now points to where the value constructors start
            if (isParametric) nextPos++;
        }

        // Create a map and a list of all the parametric types. The map is for looking them up, while the list
        // is the preserve the initial order
        Map<String, TypeRef> parametricMap = new HashMap<>();
        List<TypeRef> parametricList = new ArrayList<>();
        for (String parametric: parametricTypes) {
            TypeRef typeRef = new TypeRef();
            parametricMap.put(parametric, typeRef);
            parametricList.add(typeRef);
        }

        // Create a declaration for this type, and go ahead and define it in the type registry so it can
        // recursively refer to itself
        abstractTypeDecl = new AbstractTypeDecl(typeName, parametricList);
        SchemlRuntime.getTypeRegistry().define(abstractTypeDecl);

        // Process each value constructor
        for (Expression expr: aList.elementsFrom(nextPos)) {

            // A value constructor with no parameters can be specified with a symbol instead of a list
            if (expr instanceof SymbolExpr) {

                SymbolExpr sym = (SymbolExpr) expr;
                if (!Character.isUpperCase(sym.value.charAt(0))) {
                    throw new LispException("Value constructors should start with an upper-case letter");
                }

                // Create the value constructor with no parameters and define it in the top level
                ValueConstructorExpr valueConstructor = new ValueConstructorExpr(abstractTypeDecl.typeName, sym.value,
                        abstractTypeDecl.parametricTypes, new ArrayList<>());
                valueConstructors.add(valueConstructor);
                SchemlRuntime.getTopLevel().define(sym.value, valueConstructor);
                SchemlRuntime.getUnifyTopLevel().define(sym.value,
                        new TypeRef(new FunctionType(valueConstructor)));

            } else if (expr instanceof ListExpr) {
                ListExpr constructorExpr = (ListExpr) expr;
                Expression nameExpr = constructorExpr.getElement(0);

                if (!(nameExpr instanceof SymbolExpr)) {
                    throw new LispException("Value constructor name should be a symbol");
                }

                SymbolExpr sym = (SymbolExpr) nameExpr;
                if (!Character.isUpperCase(sym.value.charAt(0))) {
                    throw new LispException("Value constructors should start with an upper-case letter");
                }

                // Parse the type parameters for the constructor. These may contain references to this abstract type
                // or other abstract types
                List<TypeRef> typeParams = new ArrayList<>();
                for (Expression paramExpr: constructorExpr.elementsFrom(1)) {
                    typeParams.add(fromExpression(paramExpr, parametricMap));
                }

                // Create the value constructor and add it to the top level
                ValueConstructorExpr constructor = new ValueConstructorExpr(abstractTypeDecl.typeName, sym.value,
                        abstractTypeDecl.parametricTypes, typeParams);
                valueConstructors.add(constructor);
                SchemlRuntime.getTopLevel().define(sym.value, constructor);
                SchemlRuntime.getUnifyTopLevel().define(sym.value,
                        new TypeRef(new FunctionType(constructor)));
            } else {
                throw new LispException("Value constructor parameters must either be either symbols or lists");
            }
        }

        // Create a map of all the value constructors
        Map<String, ValueConstructorExpr> valueConstructorMap = new HashMap<>();
        for (ValueConstructorExpr valueConstructor: valueConstructors) {
            valueConstructorMap.put(valueConstructor.name, valueConstructor);
        }

        // Add the value constructor map to the type declaration
        abstractTypeDecl.addValueConstructors(valueConstructorMap);

        // There's nothing for this form to return
        return new VoidExpr();
    }

    /** Parses a type from a symbol, which must be one of the simple types or a type parameter like 'a */
    public static TypeRef fromSymbol(SymbolExpr sym, Map<String,TypeRef> parameterizedTypes) throws LispException {
        String name = sym.value;
        if (name.equals("bool")) {
            return new TypeRef(BooleanType.TYPE);
        } else if (name.equals("char")) {
            return new TypeRef(CharType.TYPE);
        } else if (name.equals("double")) {
            return new TypeRef(DoubleType.TYPE);
        } else if (name.equals("int")) {
            return new TypeRef(IntType.TYPE);
        } else if (name.equals("string")) {
            return new TypeRef(StringType.TYPE);
        } else if (name.equals("void")) {
            return new TypeRef(VoidType.TYPE);
        } else if (name.startsWith("'")) {
            // Make sure that this type parameter was declared in after the type name
            TypeRef parameterized = parameterizedTypes.get(name);
            if (parameterized == null) {
                throw new LispException("Parameterized type "+name+" must be declared in list after type name");
            }
            return parameterized;
        } else {
            AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().lookup(name);
            if (abstractTypeDecl != null) {
                return new TypeRef(new AbstractType(abstractTypeDecl));
            }
        }
        throw new LispException("Unknown type "+name);
    }

    /** Parses a type specification from an expression. Any complex types must be parenthesized, like (cons int)
     * and simple types like int or bool should not be.
     */
    public static TypeRef fromExpression(Expression expr, Map<String,TypeRef> parameterizedTypes) throws LispException {
        if (expr instanceof SymbolExpr) {
            // If it is just a symbol, parse the symbol
            return fromSymbol((SymbolExpr) expr, parameterizedTypes);
        } else if (!(expr instanceof ListExpr)) {
            throw new LispException("Type expression must be a symbol or a list");
        }

        ListExpr typeList = (ListExpr) expr;

        int nextPos = 0;

        // Get the next item in the expression
        Expression typeNameExpr = typeList.getElement(nextPos++);
        // If it is a symbol, it should either be cons or the type name of an abstract type
        if (typeNameExpr instanceof SymbolExpr) {

            SymbolExpr nameSym = (SymbolExpr) typeNameExpr;
            AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().lookup(nameSym.value);
            if (abstractTypeDecl != null) {
                if (abstractTypeDecl.parametricTypes.size() != typeList.size() - nextPos) {
                    throw new LispException("Parametric type list for type " + nameSym.value + " must have " +
                            abstractTypeDecl.parametricTypes.size() + " types");
                }

                // Since the list of parametric types here may contain concrete types, unify the
                // concrete type with the parametric type being referenced.
                for (int i = 0; i < abstractTypeDecl.parametricTypes.size(); i++) {
                    TypeRef absTypeSpecifier = fromExpression(typeList.getElement(nextPos++), parameterizedTypes);
                    abstractTypeDecl.parametricTypes.get(i).unify(absTypeSpecifier);
                }
                return new TypeRef(new AbstractType(abstractTypeDecl));
            }
        }

        // If we get this far then we must assume that this type specification is actually a function, so
        // you could have an abstract type that contains a function.
        nextPos = 0;

        List<TypeRef> functionParamTypes = new ArrayList<>();

        // Assume this is a function, get the type of each parameter, expecting each one to be separated
        // by a -> symbol
        while (nextPos < typeList.size()) {
            // Get the next type and add it to the function types
            TypeRef paramType = fromExpression(typeList.getElement(nextPos++), parameterizedTypes);
            functionParamTypes.add(paramType);

            // Quit if this is the end of the list
            if (nextPos >= typeList.size()) break;

            Expression shouldBeArrow = typeList.getElement(nextPos++);
            if (!(shouldBeArrow instanceof SymbolExpr)) {
                throw new LispException("Function params should be separated by ->");
            }
            if (!((SymbolExpr) shouldBeArrow).value.equals("->")) {
                throw new LispException("Function params should be separated by ->");
            }
        }

        if (functionParamTypes.size() < 2) {
            throw new LispException("Function should have at least an argument and a return type");
        }

        TypeRef[] paramTypes = new TypeRef[functionParamTypes.size() - 1];
        for (int i = 0; i < paramTypes.length; i++) paramTypes[i] = functionParamTypes.get(i);

        // Create a new type ref for a function type
        return new TypeRef(new FunctionType(paramTypes.length, paramTypes,
                functionParamTypes.get(functionParamTypes.size() - 1)));
    }
}
