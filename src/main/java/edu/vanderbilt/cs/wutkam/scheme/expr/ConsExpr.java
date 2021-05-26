package edu.vanderbilt.cs.wutkam.scheme.expr;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.runtime.Environment;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/25/21
 * Time: 9:03 AM
 */
public class ConsExpr implements Expression {
    public final Expression head;
    public final ConsExpr tail;

    public ConsExpr() {
        head = null;
        tail = null;
    }

    public ConsExpr(Expression head, ConsExpr tail) {
        this.head = head;
        this.tail = tail;
    }

    public ConsExpr(List<Expression> list) {
        ConsExpr curr = null;
        for (int i=list.size()-1; i >= 1; i--) {
            curr = new ConsExpr(list.get(i), curr);
        }
        if (list.size() > 0) {
            head = list.get(0);
            tail = curr;
        } else {
            head = null;
            tail = null;
        }
    }

    @Override
    public String toString() {
        if (head == null) return "nil";
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        ConsExpr curr = this;
        boolean first = true;
        while ((curr != null) && (curr.head != null)) {
            if (!first) builder.append(' ');
            builder.append(curr.head.toString());
            first = false;
            curr = curr.tail;
        }
        builder.append(')');
        return builder.toString();
    }
}
