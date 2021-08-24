package edu.vanderbilt.cs.wutkam.scheml.expr.builtin.sexpr;

import edu.vanderbilt.cs.wutkam.scheml.LispException;
import edu.vanderbilt.cs.wutkam.scheml.expr.*;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.BuiltinFunctionExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.FailException;
import edu.vanderbilt.cs.wutkam.scheml.parser.Parser;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.ConsTypeDecl;
import edu.vanderbilt.cs.wutkam.scheml.type.builtin.SexprTypeDecl;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;
import java.util.Stack;

/** Reads a list of lines from a file
 */
public class ParseFile extends BuiltinFunctionExpr {
    public ParseFile(String name) {
        super(name, "string -> sexpr");
    }

    @Override
    protected Expression executeBuiltin(Expression[] args) throws LispException {
        // Since this class overrides apply, executeBuiltin is never called
        return null;
    }

    @Override
    public Expression apply(List<Expression> args, Environment<Expression> env) throws LispException {
        if (args.size() < 1) {
            return new PartialApplicationExpr(this, args);
        }
        Expression filename = args.get(0);
        // Type unification will ensure the expr is a string
        try {
            File file = SchemlRuntime.getRepl().getFile(((StringExpr)filename).value);
            FileReader fileReader = new FileReader(file);
            List<Expression> expressions = Parser.parse(fileReader, false);
            Stack<Expression> exprStack = new Stack<>();
            for (Expression expr: expressions) {
                exprStack.push(SexprTypeDecl.fromExpression(expr, env));
            }
            AbstractTypeExpr curr = ConsTypeDecl.newNil();
            while (!exprStack.empty()) {
                curr = ConsTypeDecl.newCons(exprStack.pop(), curr);
            }
            return new AbstractTypeExpr("sexpr", "SexprList",
                    Arrays.asList(curr));
        } catch (IOException exc) {
            throw new FailException("Error reading file "+((StringExpr)filename).value+": "+exc.getMessage());
        }
    }
}
