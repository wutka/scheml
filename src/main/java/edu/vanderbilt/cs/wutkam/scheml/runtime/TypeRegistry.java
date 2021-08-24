package edu.vanderbilt.cs.wutkam.scheml.runtime;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheml.forms.FormExpander;
import edu.vanderbilt.cs.wutkam.scheml.parser.Parser;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.RefTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** A registry for abstract types
 */
public class TypeRegistry {
    Map<String, AbstractTypeDecl> typeMap;

    public TypeRegistry() {
        typeMap = new HashMap<>();
    }

    public void initializeBuiltinTypes() {
        typeMap.put("cons", new ConsTypeDecl());
        typeMap.put("sexpr", new SexprTypeDecl());
        typeMap.put("ref", new RefTypeDecl());
        try {
            List<Expression> exprs = Parser.parse(new InputStreamReader(
                    Thread.currentThread().getContextClassLoader().getResourceAsStream(
                            "edu/vanderbilt/cs/wutkam/scheml/stdtypes.scm")), false);
            for (Expression expr : exprs) {
                if (!(expr instanceof ListExpr)) {
                    System.out.println("Warning - unexpected value in stdtypes.scm: " + expr.toString());
                    continue;
                }
                ListExpr listExpr = (ListExpr) expr;
                if (listExpr.size() < 1) {
                    System.out.println("Warning - unexpected value in stdtypes.scm: " + expr.toString());
                    continue;
                }
                if (!(listExpr.getElement(0) instanceof SymbolExpr)) {
                    System.out.println("Warning - unexpected value in stdtypes.scm: " + expr.toString());
                    continue;
                }

                if (!((SymbolExpr) listExpr.getElement(0)).value.equals("type")) {
                    System.out.println("Warning - unexpected value in stdtypes.scm: " + expr.toString());
                    continue;
                }
                FormExpander.expand(listExpr, true);
            }
        } catch (LispException exc) {
            System.out.println("Warning - error parsing built-in types in stdtypes.scm: "+exc.getMessage());
        }
    }
    /** Locates an abstract type and returns null if it doesn't exist. If it does exist, a copy of it is
     * returned so that any changes to its type refs will not affect the copy in the global environment
     */
    public AbstractTypeDecl lookup(String name) {
        AbstractTypeDecl decl = typeMap.get(name);
        if (decl == null) return null;
        return decl.copy();
    }

    /** Stores a type definition in the registry */
    public void define(AbstractTypeDecl type) {
        typeMap.put(type.typeName, type);
    }

    /** Finds the type that a value constructor is associated with. A value constructor can only be associated
     * with a single type
     */
    public AbstractTypeDecl findByConstructor(String constructorName) {
        for (AbstractTypeDecl abstractType: typeMap.values()) {
            if (abstractType.valueConstructors.containsKey(constructorName)) {
                return abstractType.copy();
            }
        }
        return null;
    }

    public String[] getTypeNames() {
        ArrayList<String> typeNames = new ArrayList<>(typeMap.keySet());
        return typeNames.toArray(new String[typeNames.size()]);
    }
}
