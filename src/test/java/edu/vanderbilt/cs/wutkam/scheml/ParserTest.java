package edu.vanderbilt.cs.wutkam.scheml;

import edu.vanderbilt.cs.wutkam.scheml.expr.BoolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.CharExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.DoubleExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.IntExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.StringExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheml.parser.Parser;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ParserTest {

    protected void parseAndCheckValue(String sexpr, Expression expectedValue)
            throws LispException
    {
        List<Expression> exprs = Parser.parse(sexpr, false);
        assertEquals(exprs.size(), 1, "Parser should return a single expression for "+sexpr);
        assertEquals(exprs.get(0), expectedValue);
    }

    @Test
    public void testParseSimpleValues() throws LispException {
        parseAndCheckValue("5", new IntExpr(5));
        parseAndCheckValue("-5", new IntExpr(-5));
        parseAndCheckValue("5.1", new DoubleExpr(5.1));
        parseAndCheckValue("-5.1", new DoubleExpr(-5.1));
        parseAndCheckValue("#\\A", new CharExpr('A'));
        parseAndCheckValue("#t", BoolExpr.TRUE);
        parseAndCheckValue("#f", BoolExpr.FALSE);
        parseAndCheckValue("foobar", new SymbolExpr("foobar"));
        parseAndCheckValue("\"this is a \\\"string\\\"\"",
                new StringExpr("this is a \"string\""));
    }

    @Test
    public void testParseList() throws LispException {
        parseAndCheckValue("(1 2 3)", new ListExpr(
                Arrays.asList(new IntExpr(1), new IntExpr(2), new IntExpr(3))));
        parseAndCheckValue("(1 2.5 foobar \"baz\" #t)", new ListExpr(
                Arrays.asList(new IntExpr(1), new DoubleExpr(2.5),
                        new SymbolExpr("foobar"),
                        new StringExpr("baz"), BoolExpr.TRUE)));
    }

    @Test
    public void testParseFailures() {
        assertThrows(LispException.class,
                () -> Parser.parse("(1 2", false));
        assertThrows(LispException.class,
                () -> Parser.parse(")", false));
    }

    @Test
    public void testSexprs() throws LispException {
        parseAndCheckValue("`(1 2 3)",
                new ListExpr(Arrays.asList(new SymbolExpr("quote"),
                        new ListExpr(
                                Arrays.asList(new IntExpr(1),
                                    new IntExpr(2),
                                    new IntExpr(3))))));
        parseAndCheckValue("`(foo ,bar ,@baz)",
            new ListExpr(Arrays.asList(new SymbolExpr("quote"),
                new ListExpr(Arrays.asList(new SymbolExpr("foo"),
                    new ListExpr(Arrays.asList(new SymbolExpr("unquote"), new SymbolExpr("bar"))),
                    new ListExpr(Arrays.asList(new SymbolExpr("unquote-splice"), new SymbolExpr("baz"))))))));
    }
}
