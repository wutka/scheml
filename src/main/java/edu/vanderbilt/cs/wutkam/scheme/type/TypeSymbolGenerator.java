package edu.vanderbilt.cs.wutkam.scheme.type;

import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 6/1/21
 * Time: 11:38 AM
 */
public class TypeSymbolGenerator {
    Map<String,String> symbolMap;
    char nextSymbol;

    public TypeSymbolGenerator() {
        symbolMap = new HashMap<>();
        nextSymbol = 'a';
    }

    public String generateSymbol(String id) {
        String symbol = symbolMap.get(id);
        if (symbol != null) return symbol;
        if (nextSymbol >= 1000) {
            symbol = "'" + (int) nextSymbol;
        } else {
            symbol = "'" + nextSymbol;
        }
        if (nextSymbol == 'z') {
            nextSymbol = 'A';
        } else if (nextSymbol == 'Z') {
            nextSymbol = 1000;
        } else {
            nextSymbol++;
        }
        symbolMap.put(id, symbol);
        return symbol;
    }
}
