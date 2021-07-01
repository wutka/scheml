package edu.vanderbilt.cs.wutkam.scheml.forms;

public class TestFormInitializer {
    public static void initializeTestForms() {
        FormExpander.specialForms.put("expect-exception", new ExpectExceptionForm());
        FormExpander.specialForms.put("expect-exception-on-unify", new ExpectExceptionOnUnifyForm());
        FormExpander.specialForms.put("expect-no-exception", new ExpectNoExceptionForm());
        FormExpander.specialForms.put("assert-equals", new AssertThreeArgsForm());
        FormExpander.specialForms.put("assert-not-equals", new AssertThreeArgsForm());
        FormExpander.specialForms.put("assert-true", new AssertTwoArgsForm());
        FormExpander.specialForms.put("assert-false", new AssertTwoArgsForm());
        FormExpander.specialForms.put("assert-warning", new AssertTwoArgsForm());
        FormExpander.specialForms.put("assert-no-warning", new AssertTwoArgsForm());
    }
}
