package edu.vanderbilt.cs.wutkam.scheme.forms;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.TypeConstructorExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.VoidExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;
import edu.vanderbilt.cs.wutkam.scheme.type.*;

import java.util.*;

public class TypeForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 3) {
            throw new LispException("Type declaration must at least have a type name and a constructor name");
        }

        List<String> parametricTypes = new ArrayList<>();
        String typeName;
        List<TypeConstructorExpr> typeConstructors = new ArrayList<>();

        int nextPos = 1;

        Expression typeNameExpr = aList.getElement(nextPos++);
        if (typeNameExpr instanceof SymbolExpr) {
            typeName = ((SymbolExpr)typeNameExpr).value;
        } else {
            throw new LispException("Type name must be a symbol");
        }

        AbstractTypeDecl abstractTypeDecl;

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

        abstractTypeDecl = new AbstractTypeDecl(typeName, parametricList);
        SchemeRuntime.getTypeRegistry().define(abstractTypeDecl);

        for (Expression expr: aList.elementsFrom(nextPos)) {
            if (expr instanceof SymbolExpr) {
                SymbolExpr sym = (SymbolExpr) expr;
                if (!Character.isUpperCase(sym.value.charAt(0))) {
                    throw new LispException("Type constructors should start with an upper-case letter");
                }
                TypeConstructorExpr typeConstructor = new TypeConstructorExpr(abstractTypeDecl.typeName, sym.value,
                        abstractTypeDecl.parametricTypes, new ArrayList<>());
                typeConstructors.add(typeConstructor);
                SchemeRuntime.getTopLevel().define(sym.value, typeConstructor);
                SchemeRuntime.getUnifyTopLevel().define(sym.value,
                        new TypeRef(new FunctionType(typeConstructor)));
            } else if (expr instanceof ListExpr) {
                ListExpr constructorExpr = (ListExpr) expr;
                Expression nameExpr = constructorExpr.getElement(0);
                if (!(nameExpr instanceof SymbolExpr)) {
                    throw new LispException("Type constructor name should be a symbol");
                }
                SymbolExpr sym = (SymbolExpr) nameExpr;
                if (!Character.isUpperCase(sym.value.charAt(0))) {
                    throw new LispException("Type constructors should start with an upper-case letter");
                }

                List<TypeRef> typeParams = new ArrayList<>();
                for (Expression paramExpr: constructorExpr.elementsFrom(1)) {
                    typeParams.add(fromExpression(paramExpr, parametricMap));
                }

                TypeConstructorExpr constructor = new TypeConstructorExpr(abstractTypeDecl.typeName, sym.value,
                        abstractTypeDecl.parametricTypes, typeParams);
                typeConstructors.add(constructor);
                SchemeRuntime.getTopLevel().define(sym.value, constructor);
                SchemeRuntime.getUnifyTopLevel().define(sym.value,
                        new TypeRef(new FunctionType(constructor)));
            } else {
                throw new LispException("Type constructor parameters must either be either symbols or lists");
            }
        }

        Map<String,TypeConstructorExpr> typeConstructorMap = new HashMap<>();
        for (TypeConstructorExpr typeConstructor: typeConstructors) {
            typeConstructorMap.put(typeConstructor.name, typeConstructor);
        }
        abstractTypeDecl.addTypeConstructors(typeConstructorMap);

        // There's nothing for this form to return
        return new VoidExpr();
    }

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
            TypeRef parameterized = parameterizedTypes.get(name);
            if (parameterized == null) {
                throw new LispException("Parameterized type "+name+" must be declared in list after type name");
            }
            return parameterized;
        }
        throw new LispException("Unknown type "+name);
    }

    public static TypeRef fromExpression(Expression expr, Map<String,TypeRef> parameterizedTypes) throws LispException {
        if (expr instanceof SymbolExpr) {
            return fromSymbol((SymbolExpr) expr, parameterizedTypes);
        } else if (!(expr instanceof ListExpr)) {
            throw new LispException("Type expression must be a symbol or a list");
        }
        ListExpr typeList = (ListExpr) expr;

        int nextPos = 0;

        Expression typeNameExpr = typeList.getElement(nextPos++);
        if (typeNameExpr instanceof SymbolExpr) {
            SymbolExpr nameSym = (SymbolExpr) typeNameExpr;
            if (nameSym.value.equals("cons")) {
                if (typeList.size() != 2) {
                    throw new LispException("cons type should take one parameter");
                }
                TypeRef consType = fromExpression(typeList.getElement(nextPos), parameterizedTypes);
                return new TypeRef(new ConsType(consType));
            } else {
                AbstractTypeDecl abstractTypeDecl = SchemeRuntime.getTypeRegistry().lookup(nameSym.value);
                if (abstractTypeDecl != null) {
                    if (abstractTypeDecl.parametricTypes.size() != typeList.size() - nextPos) {
                        throw new LispException("Constructor for type " + nameSym.value + " must have " +
                                abstractTypeDecl.parametricTypes.size() + " parameters");
                    }
                    for (int i = 0; i < abstractTypeDecl.parametricTypes.size(); i++) {
                        TypeRef absTypeSpecifier = fromExpression(typeList.getElement(nextPos++), parameterizedTypes);
                        abstractTypeDecl.parametricTypes.get(i).unify(absTypeSpecifier);
                    }
                    return new TypeRef(new AbstractType(abstractTypeDecl));
                }
            }
        }

        nextPos = 0;

        List<TypeRef> functionParamTypes = new ArrayList<>();

        // Assume this is a function
        while (nextPos < typeList.size()) {
            TypeRef paramType = fromExpression(typeList.getElement(nextPos++), parameterizedTypes);
            functionParamTypes.add(paramType);
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

        return new TypeRef(new FunctionType(paramTypes.length, paramTypes,
                functionParamTypes.get(functionParamTypes.size() - 1)));
    }
}
