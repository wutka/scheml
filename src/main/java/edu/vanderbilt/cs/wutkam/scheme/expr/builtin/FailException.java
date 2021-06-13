package edu.vanderbilt.cs.wutkam.scheme.expr.builtin;

import edu.vanderbilt.cs.wutkam.scheme.LispException;

/** An exception to allow a fail message to be delivered up to the repl */
public class FailException extends LispException {
    public FailException() {
    }

    public FailException(String message) {
        super(message);
    }

    public FailException(String message, Throwable cause) {
        super(message, cause);
    }

    public FailException(Throwable cause) {
        super(cause);
    }

    public FailException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
