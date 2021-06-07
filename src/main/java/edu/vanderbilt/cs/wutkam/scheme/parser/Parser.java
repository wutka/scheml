package edu.vanderbilt.cs.wutkam.scheme.parser;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.*;
import edu.vanderbilt.cs.wutkam.scheme.forms.FormExpander;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class Parser {
    public List<Expression> items = new ArrayList<>();
    protected Stack<List<Expression>> expressionStack = new Stack<>();

    public static List<Expression> parse(String str) throws LispException {
        return parse(new StringReader(str));
    }

    public static List<Expression> parseWithPrompt(String str, DataInputStream dataIn) throws LispException {
        Parser parser = new Parser();
        parser.parseImpl(new StringReader(str), true, dataIn);
        return parser.items;
    }
    public static List<Expression> parse(Reader rdr) throws LispException {
        Parser parser = new Parser();
        parser.parseImpl(rdr, false, null);
        return parser.items;
    }

    protected final String symbolChars = "'*/+-!@#$%&_=:.<>?";

    protected boolean isSymbolChar(int ch) {
        return Character.isAlphabetic(ch) || symbolChars.indexOf(ch) >= 0;
    }

    protected void addExpression(Expression expr) {
        if (expressionStack.isEmpty()) {
            items.add(expr);
        } else {
            expressionStack.peek().add(expr);
        }
    }

    protected void parseImpl(Reader rdr, boolean promptForMore, DataInputStream dataIn) throws LispException {
        PushbackReader pushback = new PushbackReader(rdr);

        char ch;

        try {
            for (;;) {
                if ((ch = (char) pushback.read()) == (char) -1) {
                    if (expressionStack.isEmpty()) {
                        break;
                    }
                    if (promptForMore) {
                        System.out.print(">>>"); System.out.flush();
                        String line = dataIn.readLine();
                        pushback = new PushbackReader(new StringReader(line));
                        continue;
                    }
                    break;
                }
                if (ch == '(') {
                    expressionStack.push(new ArrayList<>());
                } else if (ch == ')') {
                    if (expressionStack.isEmpty()) {
                        throw new LispException("Got ) but there was no corresponding (");
                    }
                    List<Expression> expr = expressionStack.pop();
                    ListExpr listExpr = new ListExpr(expr);
                    if (expressionStack.isEmpty()) {
                        items.add(listExpr);
                    } else {
                        expressionStack.peek().add(listExpr);
                    }
                } else if (ch == ';') {
                    while (((ch = (char) pushback.read()) != (char) -1)) {
                        if ((ch == '\r') || (ch == '\n')) break;
                    }
                } else if (ch == '"') {
                    StringBuilder builder = new StringBuilder();
                    boolean escape = false;

                    while (((ch = (char) pushback.read()) != (char) -1)) {
                        if (escape) {
                            if (ch == 'n') {
                                builder.append('\n');
                            } else if (ch == 'r') {
                                builder.append('\r');
                            } else if (ch == 't') {
                                builder.append('\t');
                            } else {
                                builder.append(ch);
                            }
                            escape = false;
                        } else if (ch == '\\') {
                            escape = true;
                        } else if (ch == '"') {
                            break;
                        } else {
                            builder.append(ch);
                        }

                    }
                    if (ch == (char) -1) {
                        if (promptForMore) {
                            System.out.print(">>>"); System.out.flush();
                            String line = dataIn.readLine();
                            pushback = new PushbackReader(new StringReader(line));
                            continue;
                        }
                        throw new LispException("Unexpected end of stream, possible missing end-\"");
                    }
                    addExpression(new StringExpr(builder.toString()));
                } else if (isSymbolChar(ch)) {
                    if (ch == '-') {
                        int ch2 = pushback.read();
                        if (Character.isDigit(ch2)) {
                            int num = ch2-'0';
                            boolean inDouble = false;
                            double doubleFrac = 0.1;
                            double doubleNum = 0.0;
                            while (((ch = (char) pushback.read()) != (char) -1) && (Character.isDigit(ch) || ch == '.')) {
                                if (inDouble) {
                                    if (ch == '.') {
                                        throw new LispException("Extra . in double value");
                                    }
                                    doubleNum = doubleNum + ((double) (ch-'0')) * doubleFrac;
                                    doubleFrac = doubleFrac / 10.0;
                                } else if (ch == '.') {
                                    doubleNum = num;
                                    inDouble = true;
                                } else {
                                    num = num * 10 + ch - '0';
                                }
                            }
                            pushback.unread(ch);
                            Expression numValue;
                            if (inDouble) {
                                numValue = new DoubleExpr(-doubleNum);
                            } else {
                                numValue = new IntExpr(-num);
                            }
                            addExpression(numValue);
                            continue;
                        }  else {
                            pushback.unread(ch2);
                        }
                    } else if (ch == '#') {
                        char ch2 = (char) pushback.read();
                        if (ch2 == (char) -1) {
                            addExpression(new SymbolExpr("#"));
                            continue;
                        }
                        if (ch2 == '\\') {
                            char ch3 = (char) pushback.read();
                            if (ch3 == (char) -1) {
                                throw new LispException("Unexpected EOF while reading char constant");
                            }
                            addExpression(new CharExpr(ch3));
                            continue;
                        } else if (ch2 == 't') {
                            addExpression(new BoolExpr(true));
                            continue;
                        } else if (ch2 == 'f') {
                            addExpression(new BoolExpr(false));
                            continue;
                        } else {
                            pushback.unread(ch2);
                        }
                    }
                    StringBuilder builder = new StringBuilder();
                    builder.append(ch);
                    while (((ch = (char) pushback.read()) != (char) -1) && (isSymbolChar(ch) || Character.isDigit(ch))) {
                        builder.append(ch);
                    }
                    pushback.unread(ch);
                    String symbol = builder.toString();
                    if (symbol.equals("nil")) {
                        addExpression(new ConsExpr());
                    } else {
                        addExpression(new SymbolExpr(symbol));
                    }
                } else if (Character.isDigit(ch)) {
                    int num = ch - '0';
                    boolean inDouble = false;
                    double doubleNum = 0.0;
                    double doubleFrac = 0.1;
                    while (((ch = (char) pushback.read()) != (char) -1) && (Character.isDigit(ch) || ch == '.')) {
                        if (inDouble) {
                            if (ch == '.') {
                                throw new LispException("Extra . in double value");
                            }
                            doubleNum = doubleNum + ((double) (ch-'0')) * doubleFrac;
                            doubleFrac = doubleFrac / 10.0;
                        } else if (ch == '.') {
                            doubleNum = num;
                            inDouble = true;
                        } else {
                            num = num * 10 + ch - '0';
                        }
                    }
                    pushback.unread(ch);
                    Expression numValue;
                    if (inDouble) {
                        numValue = new DoubleExpr(doubleNum);
                    } else {
                        numValue = new IntExpr(num);
                    }
                    addExpression(numValue);
                } else if (!Character.isWhitespace(ch)){
                    throw new LispException("Unexpected character - "+ ch);
                }
            }
        } catch (IOException exc) {
            throw new LispException("I/O Error reading stream: "+exc.getMessage());
        }
        if (!expressionStack.isEmpty()) {
            throw new LispException("Incomplete expression, too few )'s?");
        }
    }
}
