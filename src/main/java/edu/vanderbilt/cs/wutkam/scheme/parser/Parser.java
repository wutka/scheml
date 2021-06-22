package edu.vanderbilt.cs.wutkam.scheme.parser;

import edu.vanderbilt.cs.wutkam.scheme.LispException;
import edu.vanderbilt.cs.wutkam.scheme.expr.*;
import edu.vanderbilt.cs.wutkam.scheme.type.builtin.ConsTypeDecl;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

/** Parses a stream and returns a list of expressions. The expressions should all be s-expressions.
 * The symbolChars array lists all the characters that can occur in a symbol in addition to letters and numbers */
public class Parser {
    // The expressions parsed from the string or file
    public List<Expression> items = new ArrayList<>();

    // A stack of expressions for handling nested expressions
    protected Stack<List<Expression>> expressionStack = new Stack<>();

    /** Parses a string and returns a list of all the expressions found in the string. If an expression
     * is incomplete, a LispException will be thrown. */
    public static List<Expression> parse(String str, boolean display) throws LispException {
        return parse(new StringReader(str), display);
    }

    /** Parses a string and returns a list of all the expressions found in the string. If the expression is
     * incomplete, the prompt string will be printed and an additional line of data will be read from
     * dataIn using the readLine method.
     */
    public static List<Expression> parseWithPrompt(String str, String prompt, BufferedReader dataIn,
                                                   boolean display) throws LispException {
        Parser parser = new Parser();
        parser.parseImpl(new StringReader(str), true, prompt, dataIn, display);
        return parser.items;
    }

    /** Parses expressions from a reader. If the input ends before the last expression is complete,
     * a LispException is thrown.
     */
    public static List<Expression> parse(Reader rdr, boolean display) throws LispException {
        Parser parser = new Parser();
        parser.parseImpl(rdr, false, null, null, display);
        return parser.items;
    }

    /** The characters allowed in a symbol */
    protected final String symbolChars = "'*/+-!@#$%&_=:.<>?";

    /** Returns true if a character may appear as the first character in a symbol */
    protected boolean isSymbolChar(int ch) {
        return Character.isAlphabetic(ch) || symbolChars.indexOf(ch) >= 0;
    }

    /** Adds an expression to the current expression list being created */
    protected void addExpression(Expression expr) {
        if (expressionStack.isEmpty()) {
            items.add(expr);
        } else {
            expressionStack.peek().add(expr);
        }
    }

    protected void parseImpl(Reader rdr, boolean promptForMore, String prompt, BufferedReader dataIn,
                             boolean display) throws LispException {
        PushbackReader pushback = new PushbackReader(rdr);

        char ch;

        int lineNum = 1;
        int colNum = 0;
        try {
            for (;;) {
                if ((ch = (char) pushback.read()) == (char) -1) {
                    // If end-of-file and the expressionStack is empty so we aren't waiting on an expression to
                    // be completed, return the expressions
                    if (expressionStack.isEmpty()) {
                        break;
                    }

                    // Otherwise, we were in the process of reading an expression, if promptForMore is true,
                    // print out the prompt and read another line.
                    if (promptForMore) {
                        System.out.print(prompt); System.out.flush();
                        String line = dataIn.readLine();
                        pushback = new PushbackReader(new StringReader(line));
                        continue;
                    }
                    break;
                }
                if (display) System.out.print(ch);

                if (ch == '\n') {
                    lineNum++;
                    colNum = 0;
                }
                colNum++;

                if (ch == '(') {
                    // A ( indicated the start of a subexpression
                    expressionStack.push(new ArrayList<>());

                } else if (ch == ')') {
                    // A ) closes a subexpression, and if there is no current sub-expression, that's an error
                    if (expressionStack.isEmpty()) {
                        throw new LispException("Got ) but there was no corresponding ( at line "+lineNum+" column "+colNum);
                    }

                    // We have completed the subexpression being parsed
                    List<Expression> expr = expressionStack.pop();
                    ListExpr listExpr = new ListExpr(expr);

                    // If the expression stack is empty, we have completed a top-level expression, so add it
                    // to the items
                    if (expressionStack.isEmpty()) {
                        items.add(listExpr);
                    } else {
                        // Otherwise, the subexpression was part of a larger expression, add it to the
                        // current expression being parsed
                        expressionStack.peek().add(listExpr);
                    }
                } else if (ch == ';') {
                    // A ; is a comment character, read until end of line
                    while (((ch = (char) pushback.read()) != (char) -1)) {
                        if (display) System.out.print(ch);
                        if ((ch == '\r') || (ch == '\n')) break;
                    }
                } else if (ch == '"') {
                    // A " indicates the beginning of a string
                    StringBuilder builder = new StringBuilder();
                    boolean escape = false;

                    while (((ch = (char) pushback.read()) != (char) -1)) {
                        if (display) System.out.print(ch);
                        // If the last character was a \, check for special characters and append
                        // the special character, otherwise just append the character after the \
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
                            // A \ escapes the next character
                            escape = true;
                        } else if (ch == '"') {
                            // An un-escaped " terminates the string
                            break;
                        } else {
                            // Any other characters just get appended to the string
                            builder.append(ch);
                        }

                    }
                    // If we got the EOF character while reading the string, see if we need to prompt
                    // for more input
                    if (ch == (char) -1) {
                        if (promptForMore) {
                            System.out.print(prompt); System.out.flush();
                            String line = dataIn.readLine();
                            pushback = new PushbackReader(new StringReader(line));
                            continue;
                        }
                        throw new LispException("Unexpected end of stream, possible missing end-\"");
                    }
                    // Otherwise we must have gotten here by getting the closing ", so add this
                    // string to the current expression
                    addExpression(new StringExpr(builder.toString()));
                } else if (isSymbolChar(ch)) {
                    if (ch == '-') {
                        // A symbol can begin with -, but so can an int or a double, so read the next character, and
                        // if it is a digit, assume this is a number
                        char ch2 = (char) pushback.read();
                        if (Character.isDigit(ch2)) {
                            addExpression(parseNumber(pushback, true, ch2, display));
                            if (display) System.out.print(ch2);
                            continue;
                        }  else {
                            // If the first char was '-' but the second wasn't a digit, unread the second digit, so
                            // the '-' will end up as the first character in a symbol
                            pushback.unread(ch2);
                        }
                    } else if (ch == '#') {
                        // A # could indicate the start of a symbol, but could also be a character or #t or #f
                        char ch2 = (char) pushback.read();
                        if (ch2 == (char) -1) {
                            if (display) System.out.print(ch2);
                            addExpression(new SymbolExpr("#"));
                            continue;
                        }
                        // If the second character is \, treat this as a character constant and read the next character
                        if (ch2 == '\\') {
                            if (display) System.out.print(ch2);
                            char ch3 = (char) pushback.read();
                            if (ch3 == (char) -1) {
                                throw new LispException("Unexpected EOF while reading char constant");
                            }
                            if (display) System.out.print(ch3);
                            addExpression(new CharExpr(ch3));
                            continue;
                        } else if (ch2 == 't') {
                            if (display) System.out.print(ch2);
                            // #t is the constant for true
                            addExpression(new BoolExpr(true));
                            continue;
                        } else if (ch2 == 'f') {
                            if (display) System.out.print(ch2);
                            // #f is the constant for false
                            addExpression(new BoolExpr(false));
                            continue;
                        } else {
                            // Otherwise unread the character and let it be handled by the symbol reader below
                            pushback.unread(ch2);
                        }
                    }

                    // Keep reading characters while they are valid symbol characters
                    StringBuilder builder = new StringBuilder();
                    builder.append(ch);
                    while (((ch = (char) pushback.read()) != (char) -1) && (isSymbolChar(ch) || Character.isDigit(ch))) {
                        if (display) System.out.print(ch);
                        builder.append(ch);
                    }

                    // The last character wasn't a symbol char, so unread it
                    pushback.unread(ch);

                    // Turn the builder into a string, see if it is the nil constant
                    String symbol = builder.toString();
                    if (symbol.equals("nil")) {
                        addExpression(ConsTypeDecl.newNil());
                    } else {
                        addExpression(new SymbolExpr(symbol));
                    }
                } else if (Character.isDigit(ch)) {
                    // If we get a digit, parse it as a number
                    addExpression(parseNumber(pushback, false, ch, display));
                } else if (!Character.isWhitespace(ch)){
                    // We skip whitespace, but anything else is an illegal character
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

    /** Parses a number from the input stream */
    static Expression parseNumber(PushbackReader pushback, boolean isNegative, char ch, boolean display)
            throws IOException, LispException {
        long num = ch-'0';
        boolean inDouble = false;
        double doubleFrac = 0.1;
        double doubleNum = 0.0;
        // Keep reading as long as we get digits or a decimal point
        while (((ch = (char) pushback.read()) != (char) -1) && (Character.isDigit(ch) || ch == '.')) {
            if (display) System.out.print(ch);
            if (inDouble) {
                if (ch == '.') {
                    // ... but only one decimal point
                    throw new LispException("Extra . in double value");
                }
                // If we have gotten a decimal point, we are adding on to the fractional part of
                // the number, so add it, and then make doubleFrac indicate the next digit position
                doubleNum = doubleNum + ((double) (ch-'0')) * doubleFrac;
                doubleFrac = doubleFrac / 10.0;
            } else if (ch == '.') {
                // If we get a decimal point, convert the current integer number that has been
                // accumulated into a double, and set the inDouble flag
                doubleNum = num;
                inDouble = true;
            } else {
                // Add this digit to the integer being parsed (it is converted to a double
                // at the time a '.' is read)
                num = num * 10 + ch - '0';
            }
        }
        // The most recent character read wasn't a digit or a decimal point, so unread it
        pushback.unread(ch);

        // Convert the number to an expression, remembering that we got here by seeing a '-'
        // so the number must be negative
        Expression numValue;
        if (isNegative) {
            if (inDouble) {
                numValue = new DoubleExpr(-doubleNum);
            } else {
                numValue = new IntExpr(-num);
            }
        } else {
            if (inDouble) {
                numValue = new DoubleExpr(doubleNum);
            } else {
                numValue = new IntExpr(num);
            }
        }
        return numValue;
    }
}
