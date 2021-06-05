package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheme.runtime.SchemeRuntime;

import java.io.PushbackReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public abstract class Type {
    @Override
    public String toString() {
        return toSignatureString(new TypeSymbolGenerator());
    }

    public abstract String toSignatureString(TypeSymbolGenerator gen);

    public static Type parseTypeSignature(String signature) throws LispException {
        return parseTypeSignature(signature, new HashMap<>());
    }

    public static Type parseTypeSignature(String signature, Map<String,TypeRef> symbolNameMap)
        throws LispException {
        try {
            PushbackReader pushback = new PushbackReader(new StringReader(signature));
            StringBuilder currSymbol = new StringBuilder();
            Stack<List<TypeRef>> typeStack = new Stack<>();

            typeStack.push(new ArrayList<>());

            char ch;
            while ((ch = (char) pushback.read()) != (char) -1) {
                if (ch == '-') {
                    char ch2 = (char) pushback.read();
                    if (ch2 == (char) -1) {
                        throw new LispException("Unexpected end of string after - in signature "+signature);
                    }
                    if (ch2 != '>') {
                        throw new LispException("Expected > after - in signature "+signature);
                    }

                    String symbolName = currSymbol.toString().trim();
                    typeStack.peek().add(parseSymbolName(symbolName, symbolNameMap));
                    currSymbol = new StringBuilder();
                } else if (ch == '(') {
                    typeStack.push(new ArrayList<>());
                } else if (ch == ')') {
                    List<TypeRef> nestedList = typeStack.pop();
                    if (nestedList.size() == 1) {
                        throw new RuntimeException("Parenthesized expression should be a function or a construction, "+
                                "but had only one item in signature "+signature);
                    }

                    TypeRef[] paramTypes = new TypeRef[nestedList.size()-1];
                    for (int i=0; i < paramTypes.length; i++) {
                        paramTypes[i] = nestedList.get(i);
                    }
                    TypeRef returnType = nestedList.get(nestedList.size()-1);
                    typeStack.peek().add(new TypeRef(new FunctionType(paramTypes.length, paramTypes, returnType)));
                } else {
                    currSymbol.append(ch);
                }
            }
            String symbolName = currSymbol.toString().trim();
            if (symbolName.length() > 0) {
                typeStack.peek().add(parseSymbolName(symbolName, symbolNameMap));
            }
            List<TypeRef> nestedList = typeStack.pop();
            if (nestedList.size() == 1) {
                return nestedList.get(0).getType();
            } else {
                TypeRef[] paramTypes = new TypeRef[nestedList.size()-1];
                for (int i=0; i < paramTypes.length; i++) {
                    paramTypes[i] = nestedList.get(i);
                }
                TypeRef returnType = nestedList.get(nestedList.size()-1);
                return new FunctionType(paramTypes.length, paramTypes, returnType);
            }

        } catch (Exception exc) {
            throw new LispException("Error type signature "+signature+": "+exc.getMessage(), exc);
        }
    }

    protected static TypeRef parseSymbolName(String symbolName, Map<String,TypeRef> symbolNameMap)
        throws LispException {
        String[] parts = symbolName.split(" ");

        if (parts[0].equals("cons")) {
            if (parts.length < 2) {
                throw new LispException("cons type needs a parameter in signature");
            }
            TypeRef containedType = parseSymbolName(parts[1], symbolNameMap);
            return new TypeRef(new ConsType(containedType));
        } else if (parts[0].startsWith("'")) {
            TypeRef parametricType = symbolNameMap.get(parts[0]);
            if (parametricType == null) {
                parametricType = new TypeRef();
                symbolNameMap.put(parts[0], parametricType);
            }
            return parametricType;
        } else if (parts[0].equals("bool")) {
            return new TypeRef(BooleanType.TYPE);
        } else if (parts[0].equals("char")) {
            return new TypeRef(CharType.TYPE);
        } else if (parts[0].equals("double")) {
            return new TypeRef(DoubleType.TYPE);
        } else if (parts[0].equals("int")) {
            return new TypeRef(IntType.TYPE);
        } else if (parts[0].equals("string")) {
            return new TypeRef(StringType.TYPE);
        } else if (parts[0].equals("void")) {
            return new TypeRef(VoidType.TYPE);
        }

        AbstractType abstractType = SchemeRuntime.getTypeRegistry().lookup(parts[0]);

        if (abstractType != null) {
            if (parts.length != abstractType.typeParameters.size()) {
                throw new LispException("Type constructor for type "+parts[0]+" must have "+
                        abstractType.typeParameters.size()+" type parameters");
            }
            for (int i = 1; i < parts.length; i++) {
                TypeRef part = parseSymbolName(parts[i], symbolNameMap);
                part.unify(abstractType.typeParameters.get(i-1));
            }
            return new TypeRef(abstractType);
        }
        throw new LispException("Unknown type in type signature: "+parts[0]);
    }
}
