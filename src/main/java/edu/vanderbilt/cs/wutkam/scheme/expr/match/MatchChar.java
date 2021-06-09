package edu.vanderbilt.cs.wutkam.scheme.expr.match;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.CharExpr;
import edu.vanderbilt.cs.wutkam.scheme.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheme.type.CharType;
import edu.vanderbilt.cs.wutkam.scheme.type.TypeRef;

import java.util.HashSet;
import java.util.Set;

/**
 */
public class MatchChar implements Match {
    public char value;

    protected Set<Character> charsSeen;

    public MatchChar(char value) {
        this.value = value;
    }

    @Override
    public boolean matches(Expression expression) {
        return ((CharExpr) expression).value == value;
    }

    @Override
    public void defineEnvironment(Expression expr, Environment<Expression> env) {

    }

    protected void saw(char ch) {
        if (charsSeen == null) charsSeen = new HashSet<>();
        charsSeen.add(ch);
    }

    protected char getUnseen() {
        char minChar, maxChar;
        if (charsSeen == null) return 'A';
        for (char ch=' '; ch <= '~'; ch++) {
            if (!charsSeen.contains(ch)) return ch;
        }
        char ch = 128;
        while (charsSeen.contains(ch)) ch++;
        return ch;
    }

    @Override
    public void unify(TypeRef matchTargetType, Environment<TypeRef> env) throws LispException {
        matchTargetType.unify(new TypeRef(CharType.TYPE));
    }

    @Override
    public String toString() { return (new CharExpr(value)).toString(); }
}
