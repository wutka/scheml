package edu.vanderbilt.cs.wutkam.scheml.expr;

import org.junit.jupiter.api.Test;

import java.util.List;

public class TestFormatParser {
    @Test
    public void testFormatParser() {
        try {
            List<PrintfExpr.StringFormatTypeRef> refs = PrintfExpr.parseFormatString("%d %s %b %07d %1.3f %% %n %0d");
            System.out.println();
            refs = PrintfExpr.parseFormatString("%d %2$s %<$b %07d %<$1.3f %% %n %0d");
            System.out.println();
        } catch (Exception exc) {
            exc.printStackTrace();
        }
    }
}
