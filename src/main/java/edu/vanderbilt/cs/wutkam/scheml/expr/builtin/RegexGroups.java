package edu.vanderbilt.cs.wutkam.scheml.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.AbstractTypeExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.ArrayExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Applies a regex with groups to a string and returns an array of the groups
 */
public class RegexGroups extends BuiltinFunctionExpr {
    public RegexGroups(String name) {
        super(name, "string -> string -> option (array string)");
    }

    @Override
    public Expression executeBuiltin(Expression[] args) throws LispException {
        AbstractTypeExpr curr = ConsTypeDecl.newNil();
        // Type unification ensures this will be a string

        String regexStr = ((StringExpr)args[0]).value;
        Pattern regex = Pattern.compile(regexStr);

        Matcher matcher = regex.matcher(((StringExpr)args[1]).value);
        if (!matcher.matches()) {
            return new AbstractTypeExpr("option", "Nothing", new ArrayList<>());
        }
        StringExpr[] groups = new StringExpr[matcher.groupCount()];

        for (int i=0; i < groups.length; i++) {
            groups[i] = new StringExpr(matcher.group(i+1));
        }
        ArrayExpr arr = new ArrayExpr(groups);
        return new AbstractTypeExpr("option", "Just", Arrays.asList(arr));
    }
}
