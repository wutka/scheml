package edu.vanderbilt.cs.wutkam.scheml;

import edu.vanderbilt.cs.wutkam.scheml.expr.Expression;
import edu.vanderbilt.cs.wutkam.scheml.expr.ListExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.SymbolExpr;
import edu.vanderbilt.cs.wutkam.scheml.expr.builtin.test.BuiltinTestFunctionsInitializer;
import edu.vanderbilt.cs.wutkam.scheml.forms.FormExpander;
import edu.vanderbilt.cs.wutkam.scheml.forms.TestFormInitializer;
import edu.vanderbilt.cs.wutkam.scheml.parser.Parser;
import edu.vanderbilt.cs.wutkam.scheml.runtime.Environment;
import edu.vanderbilt.cs.wutkam.scheml.runtime.SchemlRuntime;
import edu.vanderbilt.cs.wutkam.scheml.type.TypeRef;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/** Runs all the Scheml unit tests in src/test/resources/edu/vanderbilt/cs/wutkam/scheml/test
 * This class provides a list of DynamicTest objects for junit so that JUnit tests can be
 * written in Scheml. There is not currently a standalone interface in Scheml itself for JUnit,
 * so you can't currently use JUnit to test a standalone Scheml application. This mechanism
 * is for testing the Scheml implementation.
 */

public class UnitTestsInScheml {

    @TestFactory
    public Collection<DynamicTest> runUnitTests() throws IOException, LispException {
        List<DynamicTest> tests = new ArrayList<>();

        // Find all the files in the test directory
        File[] testFiles = getTestFiles();

        // Load all the files and add their tests to the dynamic test list
        for (File testFile: testFiles) {
            tests.addAll(loadTestFile(testFile));
        }
        return tests;
    }

    protected static File[] getTestFiles() {
        URL url = Thread.currentThread().getContextClassLoader().getResource(
                "edu/vanderbilt/cs/wutkam/scheml/test");
        return new File(url.getPath()).listFiles();
    }

    protected static List<DynamicTest> loadTestFile(File file) throws IOException, LispException {
        BufferedReader fileIn = new BufferedReader(new FileReader(file));

        // Parse the file
        List<Expression> exprs = Parser.parse(fileIn, false);

        fileIn.close();

        // We accumulate (define) expressions as we go, and make sure they are only visible to
        // the tests that come after them
        List<Expression> currDefinitions = new ArrayList<>();

        List<DynamicTest> tests = new ArrayList<>();

        for (Expression expr: exprs) {
            // We don't bother with expressions that aren't lists
            if (!(expr instanceof ListExpr)) continue;

            ListExpr listExpr = (ListExpr) expr;

            // We also don't bother with lists that don't start with a symbol
            if (!(listExpr.getElement(0) instanceof SymbolExpr)) {
                continue;
            }
            SymbolExpr sym = (SymbolExpr) listExpr.getElement(0);

            if (sym.value.equals("define") || sym.value.equals("type")) {
                // If this is a new definition, add it to the definitions list
                currDefinitions.add(listExpr);
                continue;
            } else if (!sym.value.equals("test")) {
                // We don't care about expressions that aren't test or define or type
                continue;
            }

            if (listExpr.size() < 3) {
                throw new LispException("Test must contain a name and at least one expression: "+listExpr);
            }

            if (!(listExpr.getElement(1) instanceof SymbolExpr)) {
                throw new LispException("Test name must be a symbol: "+listExpr);
            }

            String testName = ((SymbolExpr)listExpr.getElement(1)).value;

            List<Expression> bodyExprs = listExpr.elementsFrom(2);

            List<Expression> definitionsSoFar = new ArrayList<>(currDefinitions);

            // Create a new dynamic test for each test found in the file
            tests.add(DynamicTest.dynamicTest(testName, () -> {
                // Reset the runtime to its initial startup state
                SchemlRuntime.reset();

                // Add in testing-specific forms and built-in functions
                TestFormInitializer.initializeTestForms();
                BuiltinTestFunctionsInitializer.initializeBuiltins(SchemlRuntime.getTopLevel(),
                        SchemlRuntime.getUnifyTopLevel());

                // Make the (define) statements that occurred before this test visible to this test
                loadDefinitions(definitionsSoFar);

                // Make sure all the forms in the body are expanded
                List<Expression> body = new ArrayList<>();
                for (Expression bodyExpr: bodyExprs) {
                    if (bodyExpr instanceof ListExpr) {
                        bodyExpr = FormExpander.expand((ListExpr) bodyExpr, false);
                    }
                    body.add(bodyExpr);
                }

                executeBody(body);
            }));
        }
        return tests;
    }

    protected static void loadDefinitions(List<Expression> exprs) throws LispException {
        // Expand all the definitions in the list. There is no need to evaluate the result,
        // the important thing is that when they are expanded, they are stored in the top-level
        // environment
        for (Expression expr: exprs) {
            if (expr instanceof ListExpr) {
                FormExpander.expand((ListExpr) expr, true);
            }
        }
    }

    protected static void executeBody(List<Expression> exprs) throws LispException {
        for (Expression expr: exprs) {
            // Unify the expression first to make sure it type-checks
            expr.unify(new TypeRef(), new Environment<>());

            expr.evaluate(new Environment<>(), false);
        }
    }
}
