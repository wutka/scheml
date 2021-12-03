package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.dict;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Applicable;
import edu.vanderbilt.cs.wutkam.scheml.expr.DictExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/** Maps a function over an dict and returns a new dict */
public class DictMap extends BuiltinFunctionExpr {
    public DictMap(String name) {
        super(name, "(pair 'a 'b -> pair 'c 'd ) -> dict 'a 'b -> dict 'c 'd");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        Applicable f = (Applicable) args[0];
        DictExpr oldDict = (DictExpr) args[1];
        Map<Expression,Expression> newDict = new HashMap<>();

        for (Map.Entry<Expression,Expression> entry: oldDict.dict.entrySet()) {
            AbstractTypeExpr oldPair = new AbstractTypeExpr("pair", "Pair",
                    Arrays.asList(entry.getKey(), entry.getValue()));
            Expression newPairExpr = applyLoop(f, Arrays.asList(oldPair));
            AbstractTypeExpr newPair = (AbstractTypeExpr) newPairExpr;
            newDict.put(newPair.values.get(0), newPair.values.get(1));
        }

        return new DictExpr(newDict);
    }
}
