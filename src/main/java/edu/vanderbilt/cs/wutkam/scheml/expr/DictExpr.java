package edu.vanderbilt.cs.wutkam.scheml.expr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.type.ArrayType;
import edu.vanderbilt.cs.wutkam.scheml.type.DictType;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DictExpr implements Expression {
    public Map<Expression,Expression> dict;

    public DictExpr() {
        this.dict = new HashMap<>();
    }

    public DictExpr(Map<Expression,Expression> otherDict) {
        this.dict = otherDict;
    }

    public DictExpr(ListExpr listExpr) throws LispException {
        this.dict = new HashMap<>();

        for (Expression itemExpr: listExpr.elementsFrom(0)) {
            if (!(itemExpr instanceof ListExpr)) {
                throw new LispException("dict item is not a pair");
            }
            ListExpr itemList = (ListExpr) itemExpr;
            if (itemList.size() != 2) {
                throw new LispException("dict item is not a pair");
            }
            this.dict.put(itemList.getElement(0), itemList.getElement(1));
        }
    }

    @Override
    public void unify(TypeRef typeRef, Environment<TypeRef> env) throws LispException {
        if (dict.size() > 0) {
            for (Map.Entry<Expression,Expression> entry: dict.entrySet()) {
                Expression key = entry.getKey();
                Expression value = entry.getValue();
                TypeRef keyType = new TypeRef();
                key.unify(keyType, env);
                TypeRef valueType = new TypeRef();
                value.unify(valueType, env);
                typeRef.unify(new TypeRef(new DictType(keyType, valueType)));
            }
        } else {
            typeRef.unify(new TypeRef(new DictType(new TypeRef(), new TypeRef())));
        }
    }

    @Override
    public Expression toScheml() {
        List<Expression> scheml = new ArrayList<>();
        scheml.add(new SymbolLiteralExpr("make-dict"));
        for (Map.Entry<Expression,Expression> entry: dict.entrySet()) {
            List<Expression> keyValueList = new ArrayList<>();
            keyValueList.add(entry.getKey());
            keyValueList.add(entry.getValue());
            scheml.add(new ListExpr(keyValueList));
        }
        return new ListExpr(scheml);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("(make-dict");
        for (Map.Entry<Expression,Expression> entry: dict.entrySet()) {
            builder.append(" (");
            builder.append(entry.getKey().toString());
            builder.append(" ");
            builder.append(entry.getValue().toString());
            builder.append(")");
        }
        builder.append(')');
        return builder.toString();
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof DictExpr otherDict)) return false;
        return dict.equals(otherDict.dict);
    }
}
