package edu.vanderbilt.cs.wutkam.scheml.type;

import java.util.HashMap;
import java.util.Map;

/** When generating type symbols to represent any type, we want to generate them in sequence, starting with 'a
 * and then the next time we need a type symbol, get 'b. What happens if we get to 'z, other than that our code
 * is probably an ugly mess?  Here we go to 'A next, and then after 'Z we go to '1000, '1001, etc.
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
        // If the next symbol is >= 1000, interpret it as an int
        if (nextSymbol >= 1000) {
            symbol = "'" + (int) nextSymbol;
        } else {
            symbol = "'" + nextSymbol;
        }
        // The next symbol after 'z should be 'A
        if (nextSymbol == 'z') {
            nextSymbol = 'A';
        } else if (nextSymbol == 'Z') {
            // The next symbol after 'Z should be '1000
            nextSymbol = 1000;
        } else {
            nextSymbol++;
        }
        symbolMap.put(id, symbol);
        return symbol;
    }
}
