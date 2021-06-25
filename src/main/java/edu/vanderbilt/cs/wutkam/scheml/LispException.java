package edu.vanderbilt.cs.wutkam.scheml;

/** An exception specific to the interpreter
 */
public class LispException extends Exception {
    public LispException() {
    }

    public LispException(String message) {
        super(message);
    }

    public LispException(String message, Throwable cause) {
        super(message, cause);
    }

    public LispException(Throwable cause) {
        super(cause);
    }

    public LispException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
