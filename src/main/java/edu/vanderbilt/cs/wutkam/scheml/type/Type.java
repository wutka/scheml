package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;

import java.io.PushbackReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

/** The base class for all types. This class also has utility methods to parse type signatures from strings */
public abstract class Type {
    public void unify(Type other) throws LispException {
        if (this == other) return;
        if (getClass() != other.getClass()) {
            throw new UnifyException("Can't unify "+toString()+" with "+other.toString());
        }
    }

    public Type copy(Map<String,TypeRef> linkageMap) {
        return this;
    }

    @Override
    public String toString() {
        return toSignatureString(new TypeSymbolGenerator());
    }

    public abstract String toSignatureString(TypeSymbolGenerator gen);

    public static Type parseTypeSignature(String signature) throws LispException {
        return parseTypeSignature(signature, new HashMap<>());
    }

    /** Parse a type signature from a string, using symbolNameMap to ensure that all references to parametric
     * types are consistent - every reference to 'a will refer to the same type ref
     */
    public static Type parseTypeSignature(String signature, Map<String,TypeRef> symbolNameMap)
        throws LispException {
        try {
            PushbackReader pushback = new PushbackReader(new StringReader(signature));
            StringBuilder currSymbol = new StringBuilder();
            Stack<List<TypeRef>> typeStack = new Stack<>();

            // The type signatures can be nested, the stack keeps track of where we are at each level
            typeStack.push(new ArrayList<>());

            char ch;
            while ((ch = (char) pushback.read()) != (char) -1) {
                if (ch == '-') {
                    char ch2 = (char) pushback.read();
                    if (ch2 == (char) -1) {
                        throw new LispException("Unexpected end of string after - in signature "+signature);
                    }
                    // a - must be followed by >  (i.e. -> )
                    if (ch2 != '>') {
                        throw new LispException("Expected > after - in signature "+signature);
                    }

                    // Once we hit a -> we are done parsing the symbol we were parsing,
                    String symbolName = currSymbol.toString().trim();
                    if (symbolName.length() > 0) {
                        typeStack.peek().add(parseSymbolName(symbolName, symbolNameMap));
                        currSymbol = new StringBuilder();
                    }

                } else if (ch == '(') {
                    // A ( begins a sub expression
                    typeStack.push(new ArrayList<>());
                } else if (ch == ')') {
                    // A ) ends the current sub-expression

                    // Get the symbol that was being built before we hit the paren
                    String symbolName = currSymbol.toString().trim();
                    if (symbolName.length() > 0) {
                        typeStack.peek().add(parseSymbolName(symbolName, symbolNameMap));
                        currSymbol = new StringBuilder();
                    }

                    List<TypeRef> nestedList = typeStack.pop();
                    if (nestedList.size() == 1) {
                        throw new RuntimeException("Parenthesized expression should be a function or a construction, " +
                                "but had only one item in signature " + signature);
                    }

                    // Create a list of all the parameters
                    TypeRef[] paramTypes = new TypeRef[nestedList.size() - 1];
                    for (int i = 0; i < paramTypes.length; i++) {
                        paramTypes[i] = nestedList.get(i);
                    }

                    // Create a function type for this expression
                    TypeRef returnType = nestedList.get(nestedList.size() - 1);
                    typeStack.peek().add(new TypeRef(new FunctionType(paramTypes.length, paramTypes, returnType)));
                } else {
                    // The current symbol can contain whitespace
                    currSymbol.append(ch);
                }
            }

            // See if there is any data in the current symbol
            String symbolName = currSymbol.toString().trim();
            if (symbolName.length() > 0) {
                typeStack.peek().add(parseSymbolName(symbolName, symbolNameMap));
            }

            List<TypeRef> nestedList = typeStack.pop();
            if (nestedList.size() == 1) {
                // If the list only has one item, return its type
                return nestedList.get(0).getType();
            } else {
                // Otherwise the list represents a function, create a function definition from the param types
                TypeRef[] paramTypes = new TypeRef[nestedList.size()-1];
                for (int i=0; i < paramTypes.length; i++) {
                    paramTypes[i] = nestedList.get(i);
                }
                if ((paramTypes.length == 1) && (paramTypes[0].getType().equals(VoidType.TYPE))) {
                    paramTypes = new TypeRef[0];
                }
                TypeRef returnType = nestedList.get(nestedList.size()-1);
                return new FunctionType(paramTypes.length, paramTypes, returnType);
            }

        } catch (Exception exc) {
            throw new LispException("Error type signature "+signature+": "+exc.getMessage(), exc);
        }
    }

    /** Parses a symbol name that can include cons and abstract type declarations */
    protected static TypeRef parseSymbolName(String symbolName, Map<String,TypeRef> symbolNameMap)
        throws LispException {
        String[] parts = symbolName.split(" ");

        if (parts[0].startsWith("'")) {
            // If the symbol is a parametric type, see if it is in the map already, and if not, add it
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

        // If we get this far, the symbol must be an abstract type declaration, so look it up
        AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().lookup(parts[0]);

        if (abstractTypeDecl != null) {
            // Make sure the number of parametric types is correct
            if (parts.length-1 != abstractTypeDecl.parametricTypes.size()) {
                throw new LispException("Type constructor for type "+parts[0]+" must have "+
                        abstractTypeDecl.parametricTypes.size()+" type parameters");
            }

            // Parse the symbol names and then unify them with the abstract type
            for (int i = 1; i < parts.length; i++) {
                TypeRef part = parseSymbolName(parts[i], symbolNameMap);
                part.unify(abstractTypeDecl.parametricTypes.get(i-1));
            }
            return new TypeRef(new AbstractType(abstractTypeDecl));
        }
        throw new LispException("Unknown type in type signature: "+parts[0]);
    }
}
