package edu.vanderbilt.cs.wutkam.scheme.runtime;

import java.util.HashMap;
import java.util.Map;

/** A chained environment for symbol lookup. This environment may be used for multiple types
 * of data such as Expression or TypeRef. The chain makes it easy to create an environment temporary
 * environments that refer to symbols like those bound in a function invocation without modifying the
 * underlying environment which contains all the global symbols.
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

    /** Locates a symbol in the environment. If it isn't found in this object, we recurse down the chain
     * until we find it or hit the end of the chain.
     */
    public T lookup(String symbol) {
        T result = symbols.get(symbol);
        if (result != null) return result;
        if (next != null) return next.lookup(symbol);
        return null;
    }

    /** Returns the previous environment, which just pops the first environment off the chain. This is
     * just used for backtracking when doing tail call optimization.
     */
    public Environment<T> previous() {
        return next;
    }

    /** Returns true if this symbol is defined at the top level, which means extra care needs to be taken
     * to not mess with its type definitions.
     */
    public boolean isTopLevel(String symbol) {
        T result = symbols.get(symbol);
        if (result != null) {
            return next == null;
        }
        return next.isTopLevel(symbol);
    }

    /** Defines a symbol at this place in the environment. If that symbol exists further down the chain,
     * it is hidden from any function using this environment (other functions may have a reference to a
     * place further down the chain where that hidden symbol is still visible.
     */
    public void define(String name, T expr) {
        symbols.put(name, expr);
    }
}
