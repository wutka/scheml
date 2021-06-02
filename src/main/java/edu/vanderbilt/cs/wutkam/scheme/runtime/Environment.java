package edu.vanderbilt.cs.wutkam.scheme.runtime;

import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/24/21
 * Time: 3:14 PM
 */
public class Environment<T> {
    protected Map<String, T> symbols;
    protected Environment<T> next;

    public Environment() {
        symbols = new HashMap<>();
        next = null;
    }

    public Environment(Environment<T> next) {
        symbols = new HashMap<>();
        this.next = next;
    }

    public T lookup(String symbol) {
        T result = symbols.get(symbol);
        if (result != null) return result;
        if (next != null) return next.lookup(symbol);
        return null;
    }

    public Environment<T> previous() {
        return next;
    }

    public boolean isTopLevel(String symbol) {
        T result = symbols.get(symbol);
        if (result != null) {
            return next == null;
        }
        return next.isTopLevel(symbol);
    }

    public void define(String name, T expr) {
        symbols.put(name, expr);
    }
}
