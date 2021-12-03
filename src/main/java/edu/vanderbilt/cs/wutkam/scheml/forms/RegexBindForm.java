package edu.vanderbilt.cs.wutkam.scheml.forms;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.*;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.AbstractTypeDecl;

import java.util.ArrayList;
import java.util.List;

/** Expands the form (regex-bind target-string "regex" (var var (processing-function var) ...) body)
 * into a RegexBindExpr that applies the regex to the target string, expecting that the regex defines
 * groups, and then binds each group in sequence with the vars defined. If a group should be ignored,
 * a _ can be used as a placeholder for that group.
 * The regex string must be supplied at compile-time so it can be checked against the vars to ensure
 * that the proper number of groups exist.
 * Whatever the body returns will be wrapped in an option type, and if the regex doesn't match,
 * the expression will return Nothing.
 */
public class RegexBindForm implements Form {
    @Override
    public Expression expandForm(ListExpr aList, boolean isTopLevel) throws LispException {
        if (aList.size() < 5) {
            throw new LispException("required regex-bind format (regex-bind target \"regex\" (vars) body)");
        }

        Expression targetExpr = aList.getElement(1);
        if (targetExpr instanceof ListExpr) {
            targetExpr = FormExpander.expand((ListExpr) targetExpr, false);
        }

        Expression regexExpr = aList.getElement(2);
        if (!(regexExpr instanceof StringExpr)) {
            throw new LispException("regex-bind regex must be a constant string, not an expression");
        }
        String regex = ((StringExpr)regexExpr).value;
        int numGroups = computeRegexGroups(regex);

        Expression varsExpr = aList.getElement(3);
        if (!(varsExpr instanceof ListExpr)) {
            throw new LispException("regex-bind vars must be a non-empty list");
        }

        ListExpr varsList = (ListExpr) varsExpr;
        if (varsList.size() == 0) {
            throw new LispException("regex-bind vars must be a non-empty list");
        }

        if (varsList.size() != numGroups) {
            throw new LispException(String.format("regex-bind has %d vars, regex has %d groups",
                    varsList.size(), numGroups));
        }

        List<RegexBindExpr.RegexBindVariable> bindVariables = new ArrayList<>();
        for (Expression varExpr: varsList.elementsFrom(0)) {
            if (varExpr instanceof ListExpr) {
                ListExpr varExprList = (ListExpr) varExpr;
                if (varExprList.size() != 2) {
                    throw new LispException("regex-bind var with func should be 2-element list");
                }
                Expression funcExpr = varExprList.getElement(0);
                if (funcExpr instanceof ListExpr) {
                    funcExpr = FormExpander.expand((ListExpr) funcExpr, false);
                }
                Expression symExpr = varExprList.getElement(1);
                if (!(symExpr instanceof SymbolExpr)) {
                    throw new LispException("regex-bind var name should be a symbol");
                }
                bindVariables.add(new RegexBindExpr.RegexBindVariable(((SymbolExpr)symExpr).value, funcExpr));
            } else if (varExpr instanceof SymbolExpr) {
                bindVariables.add(new RegexBindExpr.RegexBindVariable(((SymbolExpr)varExpr).value));
            } else {
                throw new LispException("Expected symbol or (func symbol) in regex-bind variable");
            }
        }

        List<Expression> bodyList = new ArrayList<>();

        for (Expression bodyExpr: aList.elementsFrom(4)) {
            if (bodyExpr instanceof ListExpr) {
                bodyExpr = FormExpander.expand((ListExpr) bodyExpr, false);
            }
            bodyList.add(bodyExpr);
        }

        return new RegexBindExpr(targetExpr, regex, bindVariables, bodyList);
    }

    public static int computeRegexGroups(String regex) throws LispException {
        boolean escaped = false;
        int openCount = 0;
        int closeCount = 0;

        for (int i=0; i < regex.length(); i++) {
            char ch = regex.charAt(i);
            if (ch == '\\') {
                escaped = true;
                continue;
            } else if ((ch == '(') && !escaped) {
                openCount++;
            } else if ((ch == ')') && !escaped) {
                closeCount++;
            } else {
                escaped = false;
            }
        }

        if (openCount != closeCount) {
            throw new LispException("Unbalanced group parentheses in regex");
        }

        return openCount;
    }
}