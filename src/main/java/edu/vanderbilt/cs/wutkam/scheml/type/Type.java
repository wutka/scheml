package edu.vanderbilt.cs.wutkam.scheml.type;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;

import java.io.PushbackReader;
import java.io.StringReader;
import java.util.*;

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

    public abstract AbstractTypeExpr toTypeADT(TypeSymbolGenerator gen);

    public static Type parseTypeSignature(String signature) throws LispException {
        PushbackReader pushback = new PushbackReader(new StringReader(signature));

        TypeSignatureExpr signatureExpr = parseSignature(pushback, signature, new HashMap<>());

        return computeTypeSignature(signatureExpr, signature, new HashMap<>()).type;
    }

    /** Parse a type signature from a string, using symbolNameMap to ensure that all references to parametric
     * types are consistent - every reference to 'a will refer to the same type ref
     */

    static TypeRef computeTypeSignature(TypeSignatureExpr signatureExpr, String signature, Map<String,TypeRef> symbolNameMap)
        throws LispException
    {
        if (signatureExpr instanceof SignatureExprList) {
            TypeSignatureExpr[] parts = ((SignatureExprList)signatureExpr).exprs.toArray(new TypeSignatureExpr[0]);

            if (parts[0] instanceof SignatureSymbol) {
                String symbol = ((SignatureSymbol) parts[0]).symbol;
                if (symbol.startsWith("'")) {
                    if (parts.length > 1) {
                        throw new LispException("Unexpected symbol after " + symbol + " in " + signature);
                    }
                    // If the symbol is a parametric type, see if it is in the map already, and if not, add it
                    TypeRef parametricType = symbolNameMap.get(symbol);
                    if (parametricType == null) {
                        parametricType = new TypeRef();
                        symbolNameMap.put(symbol, parametricType);
                    }
                    return parametricType;
                } else if (symbol.equals("bool")) {
                    if (parts.length > 1) {
                        throw new LispException("Unexpected symbol after " + symbol + " in " + signature);
                    }
                    return new TypeRef(BooleanType.TYPE);
                } else if (symbol.equals("char")) {
                    if (parts.length > 1) {
                        throw new LispException("Unexpected symbol after " + symbol + " in " + signature);
                    }
                    return new TypeRef(CharType.TYPE);
                } else if (symbol.equals("double")) {
                    if (parts.length > 1) {
                        throw new LispException("Unexpected symbol after " + symbol + " in " + signature);
                    }
                    return new TypeRef(DoubleType.TYPE);
                } else if (symbol.equals("int")) {
                    if (parts.length > 1) {
                        throw new LispException("Unexpected symbol after " + symbol + " in " + signature);
                    }
                    return new TypeRef(IntType.TYPE);
                } else if (symbol.equals("bignum")) {
                    if (parts.length > 1) {
                        throw new LispException("Unexpected symbol after " + symbol + " in " + signature);
                    }
                    return new TypeRef(BignumType.TYPE);
                } else if (symbol.equals("string")) {
                    if (parts.length > 1) {
                        throw new LispException("Unexpected symbol after " + symbol + " in " + signature);
                    }
                    return new TypeRef(StringType.TYPE);
                } else if (symbol.equals("symbol")) {
                    if (parts.length > 1) {
                        throw new LispException("Unexpected symbol after " + symbol + " in " + signature);
                    }
                    return new TypeRef(SymbolType.TYPE);
                } else if (symbol.equals("void")) {
                    if (parts.length > 1) {
                        throw new LispException("Unexpected symbol after " + symbol + " in " + signature);
                    }
                    return new TypeRef(VoidType.TYPE);
                } else if (symbol.equals("array")) {
                    if (parts.length > 2) {
                        throw new LispException("Too many type names after array");
                    }
                    return new TypeRef(new ArrayType(computeTypeSignature(
                            parts[1], signature, symbolNameMap)));
                } else if (symbol.equals("dict")) {
                    if (parts.length > 3) {
                        throw new LispException("Too many type names after dict");
                    }
                    return new TypeRef(new DictType(
                            computeTypeSignature(parts[1], signature, symbolNameMap),
                            computeTypeSignature(parts[2], signature, symbolNameMap)));
                } else if (symbol.equals("set")) {
                    if (parts.length > 2) {
                        throw new LispException("Too many type names after set");
                    }
                    return new TypeRef(new SetType(
                            computeTypeSignature(parts[1], signature, symbolNameMap)));
                }

                // If we get this far, the symbol must be an abstract type declaration, so look it up
                AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().lookup(symbol);

                if (abstractTypeDecl != null) {
                    // Make sure the number of parametric types is correct
                    if (parts.length - 1 != abstractTypeDecl.parametricTypes.size()) {
                        throw new LispException("Value constructor for type " + symbol + " must have " +
                                abstractTypeDecl.parametricTypes.size() + " type parameters");
                    }

                    // Parse the symbol names and then unify them with the abstract type
                    for (int i = 1; i < parts.length; i++) {
                        TypeRef part = computeTypeSignature(
                                parts[i], signature, symbolNameMap);
                        part.unify(abstractTypeDecl.parametricTypes.get(i - 1));
                    }
                    return new TypeRef(new AbstractType(abstractTypeDecl));
                }
                throw new LispException("Unknown type in type signature: " + symbol);
            } else {
                throw new LispException("Unable to parse type signature: "+signature);
            }
        } else if (signatureExpr instanceof FunctionSignature) {
            FunctionSignature funcSig = (FunctionSignature) signatureExpr;
            if (funcSig.signatures.size() == 1) {
                throw new LispException("Function signature only has one parameter: " + signature);
            } else {
                TypeRef[] params = new TypeRef[funcSig.signatures.size() - 1];
                for (int i = 0; i < funcSig.signatures.size() - 1; i++) {
                    params[i] = computeTypeSignature(funcSig.signatures.get(i), signature, symbolNameMap);
                }

                TypeRef returnType = computeTypeSignature(funcSig.signatures.get(funcSig.signatures.size() - 1),
                        signature, symbolNameMap);
                return new TypeRef(new FunctionType(params.length, params, returnType));
            }
        } else if (signatureExpr instanceof SignatureSymbol) {
            String symbol = ((SignatureSymbol)signatureExpr).symbol;

            if (symbol.startsWith("'")) {
                // If the symbol is a parametric type, see if it is in the map already, and if not, add it
                TypeRef parametricType = symbolNameMap.get(symbol);
                if (parametricType == null) {
                    parametricType = new TypeRef();
                    symbolNameMap.put(symbol, parametricType);
                }
                return parametricType;
            } else if (symbol.equals("bool")) {
                return new TypeRef(BooleanType.TYPE);
            } else if (symbol.equals("char")) {
                return new TypeRef(CharType.TYPE);
            } else if (symbol.equals("double")) {
                return new TypeRef(DoubleType.TYPE);
            } else if (symbol.equals("int")) {
                return new TypeRef(IntType.TYPE);
            } else if (symbol.equals("bignum")) {
                return new TypeRef(BignumType.TYPE);
            } else if (symbol.equals("string")) {
                return new TypeRef(StringType.TYPE);
            } else if (symbol.equals("symbol")) {
                return new TypeRef(SymbolType.TYPE);
            } else if (symbol.equals("void")) {
                return new TypeRef(VoidType.TYPE);
            } else if (symbol.equals("array")) {
                throw new LispException("array requires a type parameter in "+signature);
            } else if (symbol.equals("dict")) {
                throw new LispException("dict requires two type parameters in "+signature);
            } else {
                // If we get this far, the symbol must be an abstract type declaration, so look it up
                AbstractTypeDecl abstractTypeDecl = SchemlRuntime.getTypeRegistry().lookup(symbol);

                if (abstractTypeDecl != null) {
                    // Make sure the number of parametric types is correct
                    if (abstractTypeDecl.parametricTypes.size() != 0) {
                        throw new LispException("Value constructor for type " + symbol + " requires no type parameters");
                    }

                    return new TypeRef(new AbstractType(abstractTypeDecl));
                } else {
                    throw new LispException("Unknown type name "+symbol+" in "+signature);
                }
            }
        } else {
            throw new LispException("Unknown signature expression type: "+signatureExpr.getClass().getName());
        }
    }

    static TypeSignatureExpr parseSignature(PushbackReader pushback, String signature, Map<String,TypeRef> symbolNameMap)
        throws LispException
    {
        List<TypeSignatureExpr> currExprList = new ArrayList<>();
        FunctionSignature funcSignature = null;

        try {
            StringBuilder currSymbol = new StringBuilder();

            char ch;
            while ((ch = (char) pushback.read()) != (char) -1) {
                if (ch == '-') {
                    char ch2 = (char) pushback.read();
                    if (ch2 == (char) -1) {
                        throw new LispException("Unexpected end of string after - in signature "+signature);
                    }
                    // a - must be followed by >  (i.e. -> )
                    if (ch2 != '>') {
                        currSymbol.append(ch);
                        pushback.unread(ch2);
                        continue;
                    }

                    // Once we hit a -> we are done parsing the symbol we were parsing,
                    String symbolName = currSymbol.toString().trim();
                    if (symbolName.length() > 0) {
                        currExprList.add(new SignatureSymbol(currSymbol.toString().trim()));
                        currSymbol = new StringBuilder();
                    }
                    if ((funcSignature == null) && currExprList.size() == 0) {
                        throw new LispException("Got -> before any type names in signature: " + signature);
                    }
                    if (funcSignature == null) {
                        funcSignature = new FunctionSignature();
                    }
                    if (currExprList.size() == 1) {
                        funcSignature.signatures.add(currExprList.get(0));
                    } else if (currExprList.size() > 1) {
                        funcSignature.signatures.add(new SignatureExprList(currExprList));
                    }
                    currExprList = new ArrayList<>();
                }
                else if (ch == '(') {
                    TypeSignatureExpr subExpr = parseSignature(pushback, signature, symbolNameMap);

                    currExprList.add(subExpr);
                } else if (ch == ')') {
                    if (currSymbol.length() > 0) {
                        if (funcSignature != null) {
                            funcSignature.signatures.add(new SignatureSymbol(currSymbol.toString().trim()));
                            return funcSignature;
                        } else {
                            currExprList.add(new SignatureSymbol(currSymbol.toString().trim()));
                            SignatureExprList exprList = new SignatureExprList(currExprList);
                            return exprList;
                        }
                    } else {
                        if (funcSignature != null) {
                            if (currExprList.size() > 0) {
                                funcSignature.signatures.add(new SignatureExprList(currExprList));
                            }
                            return funcSignature;
                        } else {
                            return new SignatureExprList(currExprList);
                        }
                    }
                } else if (ch == ' ') {
                    if (currSymbol.length() > 0) {
                        currExprList.add(new SignatureSymbol(currSymbol.toString()));
                        currSymbol = new StringBuilder();
                    }
                } else {
                    currSymbol.append(ch);
                }
            }
            if (currSymbol.length() > 0) {
                currExprList.add(new SignatureSymbol(currSymbol.toString().trim()));
            }

            SignatureExprList sigList = new SignatureExprList(currExprList);

            if (funcSignature != null) {
                if (sigList.exprs.size() > 0) {
                    funcSignature.signatures.add(sigList);
                }
                return funcSignature;
            } else if (currExprList.size() > 0) {
                return sigList;
            } else {
                throw new LispException("Empty symbol list in signature: "+signature);
            }

        } catch (Exception exc) {
            throw new LispException("Error type signature " + signature + ": " + exc.getMessage(), exc);
        }
    }

    static class TypeSignatureExpr {

    }

    static class FunctionSignature extends TypeSignatureExpr {
        List<TypeSignatureExpr> signatures;

        public FunctionSignature() {
            signatures = new ArrayList<>();
        }
    }

    static class SignatureExprList extends TypeSignatureExpr {
        List<TypeSignatureExpr> exprs;

        public SignatureExprList() {
            exprs = new ArrayList<>();
        }

        public SignatureExprList(String symbol) {
            this.exprs = Arrays.asList(new SignatureSymbol(symbol));
        }

        public SignatureExprList(List<TypeSignatureExpr> exprs) {
            this.exprs = exprs;
        }
    }

    static class SignatureSymbol extends TypeSignatureExpr {
        String symbol;

        public SignatureSymbol(String symbol) {
            this.symbol = symbol;
        }
    }
}
